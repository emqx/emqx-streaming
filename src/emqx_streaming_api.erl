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

-module(emqx_streaming_api).

-include("streaming.hrl").

-rest_api(#{name   => create_stream,
            method => 'POST',
            path   => "/streams/",
            func   => create_stream,
            descr  => "Create a stream"
           }).

-rest_api(#{name   => list_streams,
            method => 'GET',
            path   => "/streams/",
            func   => list_streams,
            descr  => "A list of all streams"
           }).

-rest_api(#{name   => show_stream,
            method => 'GET',
            path   => "/streams/:id",
            func   => show_stream,
            descr  => "Show a stream"
           }).

-rest_api(#{name   => delete_stream,
            method => 'DELETE',
            path   => "/streams/:id",
            func   => delete_stream,
            descr  => "Delete a stream"
           }).

-export([ create_stream/2
        , list_streams/2
        , show_stream/2
        , delete_stream/2
        ]).

%%------------------------------------------------------------------------------
%% Streams Management API
%%------------------------------------------------------------------------------

create_stream(_Bindings, Params) ->
    case emqx_streaming:create_stream(Params) of
        {ok, Stream} ->
            emqx_mgmt:return({ok, to_map(Stream)});
        {error, Reason} ->
            emqx_mgmt:return({error, 500, Reason})
    end.

list_streams(_Bindings, _Params) ->
    return_all(emqx_streaming:get_streams()).

show_stream(#{id := Id}, _Params) ->
    reply_with(fun emqx_streaming:get_stream/1, Id).

delete_stream(#{id := Id}, _Params) ->
    ok = emqx_streaming:remove_stream(Id),
    emqx_mgmt:return().

return_all(Records) ->
    Data = lists:map(fun to_map/1, Records),
    emqx_mgmt:return({ok, Data}).

reply_with(Find, Id) ->
    case Find(Id) of
        {ok, R} ->
            emqx_mgmt:return({ok, to_map(R)});
        not_found ->
            emqx_mgmt:return({error, 404, "Not Found"})
    end.

to_map(#stream{id = Id,
               name = Name,
               rawsql = RawSql,
               enabled = Enabled,
               description = Descr}) ->
    #{id => Id,
      name => Name,
      rawsql => RawSql,
      enabled => Enabled,
      description => Descr}.

