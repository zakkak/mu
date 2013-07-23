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

#include "mu-index.h"
#include "mu-query.h"
#include "mu-store.h"

#ifndef __MU_CMD_SERVER_H__
#define __MU_CMD_SERVER_H__

#include <glib.h>

G_BEGIN_DECLS

struct _ServerContext {
  MuStore *store;
  MuQuery *query;
  gchar   *home_dir;
};
typedef struct _ServerContext ServerContext;

extern gboolean MU_TERMINATE;
extern void install_sig_handler (void);

extern void (*send_expr) (const char *frm, ...);
extern void (*send_expr_oob) (const char *frm, ...);
extern MuError (*send_error) (MuError errcode, const char *msg);
extern MuError (*send_and_clear_g_error) (GError **err);

extern MuError handle_args (ServerContext *ctx, GHashTable *args, GError **err);

extern MuError (*index_msg_cb) (MuIndexStats *stats, void *user_data);

G_END_DECLS

#endif /* __MU_CMD_SERVER_H__ */
