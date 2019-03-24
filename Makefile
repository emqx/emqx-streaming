PROJECT = emqx_streaming
PROJECT_DESCRIPTION = EMQ X Streaming
PROJECT_VERSION = 0.1.0

NO_AUTOPATCH = cuttlefish

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)

DEPS = sqlparse emqx_rule_engine
dep_sqlparse = git-emqx https://github.com/emqx/sqlparse master
dep_emqx_rule_engine = git-emqx https://github.com/emqx/emqx-rule-engine develop

TEST_DEPS = emqx_ct_helpers
dep_emqx_ct_helpers = git-emqx https://github.com/emqx/emqx-ct-helpers develop

ERLC_OPTS += +debug_info
ERLC_OPTS += +warnings_as_errors +warn_export_all +warn_unused_import

EUNIT_OPTS = verbose

CT_SUITES = emqx_streaming
COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)

include erlang.mk

app:: rebar.config
