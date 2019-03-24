%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_streaming).

-behaviour(gen_server).

-include("streaming.hrl").

-export([start_link/0]).

%% API
-export([ get_streams/0
        , get_stream/1
        , remove_stream/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% Mnesia bootstrap
-export([mnesia/1]).

-boot_mnesia({mnesia, [boot]}).
-copy_mnesia({mnesia, [copy]}).

%% Tables
-define(STREAM_TAB, emqx_stream).

-record(state, {}).

%%------------------------------------------------------------------------------
%% Mnesia bootstrap
%%------------------------------------------------------------------------------

%% @doc Create or replicate tables.
-spec(mnesia(boot | copy) -> ok).
mnesia(boot) ->
    %% Optimize storage
    StoreProps = [{ets, [{read_concurrency, true}]}],
    %% Stream table
    ok = ekka_mnesia:create_table(?STREAM_TAB, [
                {disc_copies, [node()]},
                {record_name, stream},
                {attributes, record_info(fields, stream)},
                {storage_properties, StoreProps}]);

mnesia(copy) ->
    %% Copy stream table
    ok = ekka_mnesia:copy_table(?STREAM_TAB).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_streams() ->
    get_all_records(?STREAM_TAB).

get_stream(StreamId) ->
    case mnesia:dirty_read(?STREAM_TAB, StreamId) of
        [Rule] -> {ok, Rule};
        [] -> not_found
    end.

remove_stream(StreamId) ->
    trans(fun delete_stream/1, [StreamId]).

%% @private
delete_stream(StreamId) ->
    mnesia:delete({?STREAM_TAB, StreamId}).

get_all_records(Tab) ->
    mnesia:dirty_match_object(Tab, mnesia:table_info(Tab, wild_pattern)).

trans(Fun, Args) ->
    case mnesia:transaction(Fun, Args) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> error(Reason)
    end.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(Req, _From, State) ->
    logger:error("[RuleRegistry]: unexpected call - ~p", [Req]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    logger:error("[RuleRegistry]: unexpected cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    logger:error("[RuleRegistry]: unexpected info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------


