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

-module(emqx_streaming_cli).

-include("streaming.hrl").

-export([ load/0
        , cmd/1
        , unload/0
        ]).

-spec(load() -> ok).
load() ->
    emqx_ctl:register_command(streams, {?MODULE, cmd}, []).

-spec(unload() -> ok).
unload() ->
    emqx_ctl:unregister_command(streams).

cmd(["list"]) ->
    print_all(emqx_streaming:get_streams());

cmd(["create", Args]) ->
   emqx_cli:print("TODO create-stream: ~p~n", [Args]);

cmd(["show", Id]) ->
    print_with(fun emqx_streaming:get_stream/1, Id);

cmd(["delete", Id]) ->
    ok = emqx_streaming:remove_stream(Id),
    emqx_cli:print("Stream ~s is deleted.~n", [Id]);

cmd(_usage) ->
    emqx_cli:usage([{"streams list", "List all streams"},
                    {"streams show <Id>", "Show a stream"},
                    {"streams create", "Create a stream"},
                    {"streams delete <Id>", "Delete a stream"}
                   ]).

print_all(Records) ->
    lists:foreach(fun(Record) -> print(Record) end, Records).

print_with(FindFun, Key) ->
    case FindFun(Key) of
        {ok, R} -> print(R);
        not_found ->
            emqx_cli:print("Cannot found ~s~n", [Key])
    end.

print(#stream{id = Id, name = Name, rawsql = RawSQL,
              enabled = Enabled, description = Descr}) ->
    emqx_cli:print("stream(~s, name=~s, sql=~s, enabled=~s, description=~s~n",
                   [Id, Name, RawSQL, Enabled, Descr]).

