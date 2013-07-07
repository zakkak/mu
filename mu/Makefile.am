## Copyright (C) 2010-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software Foundation,
## Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

include $(top_srcdir)/gtest.mk

# enforce compiling guile (optionally) first,then this dir first
# before decending into tests/
SUBDIRS= . tests

AM_CPPFLAGS=-I${top_srcdir}/lib $(GLIB_CFLAGS) $(GIO_CFLAGS) $(GIO_UNIX_CFLAGS)

# don't use -Werror, as it might break on other compilers
# use -Wno-unused-parameters, because some callbacks may not
# really need all the params they get
AM_CFLAGS=-Wall -Wextra -Wno-unused-parameter -Wdeclaration-after-statement \
	-pedantic -Wno-variadic-macros -Wno-extra-semi                      \
        -DMU_SCRIPTS_DIR="\"$(pkgdatadir)/scripts/\""
AM_CXXFLAGS=-Wall -Wextra -Wno-unused-parameter

if BUILD_DBUS
dbus_sources=mu-cmd-dbus.c
dbus_built_sources=		\
	mu-server-dbus-glue.c	\
	mu-server-dbus-glue.h	\
	mu-server-dbus.xml.h
else
dbus_built_sources=
dbus_sources=
endif


bin_PROGRAMS=				\
	mu

# note, mu.cc is only '.cc' and not '.c' because libmu must explicitly
# be linked as c++, not c.
mu_SOURCES=				\
	mu.cc				\
	mu-cmd-cfind.c			\
	mu-config.c			\
	mu-config.h			\
	mu-cmd-extract.c		\
	mu-cmd-find.c			\
	mu-cmd-index.c			\
	mu-cmd-server.c			\
	mu-cmd-server.h			\
	$(dbus_sources)			\
	mu-cmd-script.c			\
	mu-cmd.c			\
	mu-cmd.h			\
	$(BUILT_SOURCES)

BUILT_SOURCES=				\
	mu-help-strings.h		\
	$(dbus_built_sources)

mu-cmd-dbus.c: $(dbus_built_sources)

mu-server-dbus-glue.c: mu-server-dbus-glue.h
mu-server-dbus-glue.h: mu-server-dbus.xml
	$(AM_V_GEN) ${GDBUS_CODEGEN} --generate-c-code          \
        ${builddir}/mu-server-dbus-glue --c-namespace=Mu	\
         --interface-prefix=nl.djcbsoftware.Mu			\
	 --c-generate-object-manager				\
         ${srcdir}/mu-server-dbus.xml
mu-server-dbus.xml.h: mu-server-dbus.xml
	$(AM_V_GEN) ${AWK} '{gsub(/"/,"\\\"",$$0); print "\"" $$0 "\"";}' < $< > $@

mu-help-strings.h: mu-help-strings.txt mu-help-strings.awk
	$(AWK) -f ${top_srcdir}/mu/mu-help-strings.awk < $< > $@

mu_LDADD=				\
	${top_builddir}/lib/libmu.la    \
	$(GLIB_LIBS)                    \
	$(GIO_LIBS)                     \
	$(GIO_UNIX_LIBS)

EXTRA_DIST=				\
	mu-help-strings.awk		\
	mu-help-strings.txt		\
	mu-server-dbus.xml

CLEANFILES=$(BUILT_SOURCES)
