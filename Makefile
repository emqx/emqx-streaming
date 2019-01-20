PROJECT = emqx_streaming
PROJECT_DESCRIPTION = EMQ X Streaming
PROJECT_VERSION = 0.0.1

ERLC_OPTS += +debug_info
ERLC_OPTS += +warnings_as_errors +warn_export_all +warn_unused_import

EUNIT_OPTS = verbose

CT_SUITES = emqx_streaming
COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)

include erlang.mk

app:: rebar.config
