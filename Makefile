PROJECT = aui_web_backend
PROJECT_DESCRIPTION = An erlang backend for generating spectra icons and data layers for ASPECT-ui.
PROJECT_VERSION = 0.1.0

DEPS = cowboy epgsql
dep_cowboy_commit = 2.4.0

DEP_PLUGINS = cowboy epgsql helpers

LOCAL_DEPS = crypto

include erlang.mk
