/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2011-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation; either version 3, or (at your option) any
** later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software Foundation,
** Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/

#include <string.h>

#include "mu-cmd.h"
#include "mu-cmd-server.h"
#include "mu-str.h"

#include "mu-cmd-dbus-generated.h"

/* ---------------------------------------------------------------------------------------------------- */

extern gboolean mu_must_terminate ();
extern void install_sig_handler (void);

static GDBusObjectManagerServer *dbus_object_manager;
static GMainLoop *dbus_loop;
static GString *dbus_buffer = NULL;

static void G_GNUC_PRINTF(1, 2)
	marshall_dbus_expr (const char *frm, ...)
{
	va_list ap;

	va_start (ap, frm);
	g_string_append_vprintf (dbus_buffer, frm, ap);
	va_end (ap);
}

static MuError
marshall_dbus_error (MuError errcode, const char *msg)
{
	char *str;

	str = mu_str_escape_c_literal (msg, TRUE);
	g_string_append_printf (dbus_buffer, "(:error %u :message %s)", errcode, str);
	g_free (str);

	return errcode;
}

static MuError
marshall_dbus_and_clear_g_error (GError **err)
{
	MuError rv;

	if (err && *err)
		rv = marshall_dbus_error ((*err)->code, (*err)->message);
	else
		rv = marshall_dbus_error (MU_ERROR_INTERNAL, "unknown error");

	g_clear_error (err);

	return rv;
}

static gboolean
on_maildirmanager_execute (MuServerMaildirManager *md_mgr,
			   GDBusMethodInvocation  *invocation,
			   gchar                  *payload,
			   gpointer                user_data)
{
	GHashTable *args;
	GError *my_err = NULL;
	ServerContext *ctx_ptr;
	gchar *char_data;

	if (MU_TERMINATE) {
		g_main_loop_quit (dbus_loop);
		/* we have not handled this request */
		return FALSE;
	}

	ctx_ptr = (ServerContext*) user_data;

	dbus_buffer = g_string_sized_new (512);

	/* args will receive a the command as a hashtable.
	 * returning NULL indicates an error, but
	 * we let handle_args() detect that. */
	args   = mu_str_parse_arglist (payload, &my_err);
	if ((!args || g_hash_table_size(args) == 0) && !my_err) {
		if (args)
			g_hash_table_destroy (args);
		/* we have not handled this request */
		return FALSE;
	} else if (my_err) {
		/* we have not handled this request */
		send_and_clear_g_error (&my_err);
		return FALSE;
	}

	switch (handle_args (ctx_ptr, args, &my_err)) {
	case MU_OK:
		break;
	case MU_STOP:
		g_main_loop_quit (dbus_loop);
		MU_TERMINATE = TRUE;
		break;
	default: /* some error occurred */
		send_and_clear_g_error (&my_err);
	}

	g_hash_table_destroy (args);

	char_data = g_string_free (dbus_buffer, FALSE);
	mu_server_maildir_manager_complete_execute (md_mgr, invocation, char_data);
	g_free(char_data);

	/* we have handled this request */
	return TRUE;
}

static void
setup_maildir_manager_signal_callbacks(MuServerMaildirManager *md_mgr,
				       gpointer user_data)
{
	g_signal_connect (md_mgr, "handle-execute",
			  G_CALLBACK (on_maildirmanager_execute),
			  user_data);
}

static void
on_bus_acquired (GDBusConnection *connection,
                 const gchar *name,
                 gpointer user_data)
{
	MuServerObjectSkeleton *object;
	MuServerMaildirManager *md_mgr;
	gchar *s;

	s = g_strdup ("/mu/maildir");
	object = mu_server_object_skeleton_new (s);
	g_free (s);

	md_mgr = mu_server_maildir_manager_skeleton_new ();
	mu_server_object_skeleton_set_maildir_manager (object, md_mgr);
	setup_maildir_manager_signal_callbacks(md_mgr, user_data);
	g_object_unref (md_mgr);

	dbus_object_manager = g_dbus_object_manager_server_new ("/mu");
	g_dbus_object_manager_server_export (dbus_object_manager,
					     G_DBUS_OBJECT_SKELETON (object));
	g_object_unref (object);

	g_dbus_object_manager_server_set_connection (dbus_object_manager,
						     connection);
}

/**
 * Strip all characters matching C from a string.  This function
 * updates the array in place.  The caller is responsible for sending
 * a modifiable array.
 */
static void
strip_chars(gchar *str, char c)
{
	size_t i, j, len;
	len = strlen (str);
	for (i=j=0; j<len; ++j) {
		if (str[j] != '/') {
			str[i++] = str[j];
		}
	}
	str[i] = '\0';
}

/**
 * Construct an object name reflecting the home directory of this mu
 * server.  Caller is responsible for calling g_free() on the return
 * value.
 */
static gchar *
construct_object_name (const gchar *home)
{
	if (home) {
		gchar *to_strip, *rv;
		to_strip = g_strdup (home);
		strip_chars (to_strip, '/');
		rv = g_strconcat ("org.example.mail.mu.Maildir",
				  ".", to_strip, NULL);
		g_free (to_strip);
		return rv;
	}
	else {
		return g_strdup ("org.example.mail.mu.Maildir");
	}
}

MuError
mu_cmd_dbus (MuStore *store, MuConfig *opts, GError **err)
{
	ServerContext ctx;
	gchar *object_name;
	guint id;

	send_expr = marshall_dbus_expr;
	send_expr_oob = marshall_dbus_expr;
	send_error = marshall_dbus_error;
	send_and_clear_g_error = marshall_dbus_and_clear_g_error;

	g_return_val_if_fail (store, MU_ERROR_INTERNAL);

	ctx.store = store;
	ctx.query = mu_query_new (store, err);
	if (!ctx.query)
		return MU_G_ERROR_CODE (err);

	install_sig_handler ();

	dbus_loop = g_main_loop_new (NULL, FALSE);

	object_name = construct_object_name (opts->muhome);

	id = g_bus_own_name (G_BUS_TYPE_SESSION,
			     object_name,
			     G_BUS_NAME_OWNER_FLAGS_NONE,
			     on_bus_acquired,
			     NULL,
			     NULL,
			     &ctx,
			     NULL);

	g_main_loop_run (dbus_loop);

	g_bus_unown_name (id);
	g_main_loop_unref (dbus_loop);
	g_free (object_name);


	mu_store_flush   (ctx.store);
	mu_query_destroy (ctx.query);

	return MU_OK;
}
