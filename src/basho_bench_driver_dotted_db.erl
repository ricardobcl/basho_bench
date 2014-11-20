%% -------------------------------------------------------------------
%%
%% basho_bench: Benchmarking Suite
%%
%% Copyright (c) 2009-2010 Basho Techonologies
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(basho_bench_driver_dotted_db).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-record(state, {client}).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    %% Make sure the path is setup such that we can get at dotted_db
    case code:which(dotted_db) of
        non_existing ->
            ?FAIL_MSG("~s requires dotted_db module to be available on code path.\n",
                      [?MODULE]);
        _ ->
            ok
    end,

    Nodes   = basho_bench_config:get(dotted_db_nodes),
    Cookie  = basho_bench_config:get(dotted_db_cookie, 'dotted_db'),
    MyNode  = basho_bench_config:get(dotted_db_mynode, [basho_bench, longnames]),
    % Replies = basho_bench_config:get(dotted_db_replies, 2),
    % Bucket  = basho_bench_config:get(dotted_db_bucket, <<"test">>),

    %% Try to spin up net_kernel
    case net_kernel:start(MyNode) of
        {ok, _} ->
            ?INFO("Net kernel started as ~p\n", [node()]);
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p\n", [?MODULE, Reason])
    end,

    %% Initialize cookie for each of the nodes
    [true = erlang:set_cookie(N, Cookie) || N <- Nodes],

    %% Try to ping each of the nodes
    ping_each(Nodes),

    %% Choose the node using our ID as a modulus
    TargetNode = lists:nth((Id rem length(Nodes)+1), Nodes),
    ?INFO("Using target node ~p for worker ~p\n", [TargetNode, Id]),

    case dotted_db:new_client(TargetNode) of
        {ok, Client} ->
            {ok, #state { client = Client }};
        {error, Reason2} ->
            ?FAIL_MSG("Failed get a dotted_db:new_client to ~p: ~p\n", [TargetNode, Reason2])
    end.

run(get, KeyGen, _ValueGen, State) ->
    case (State#state.client):get(KeyGen()) of
        {ok, not_found} ->
            {ok, State};
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(put, KeyGen, ValueGen, State) ->
    Obj = [KeyGen(), ValueGen()],
    case (State#state.client):put(Obj) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(update, KeyGen, ValueGen, State) ->
    Key = KeyGen(),
    NewValue = ValueGen(),
    case (State#state.client):get(Key) of
        {ok, {_Values, Context}} ->
            UpdatedObj = [Key, NewValue, Context],
            case (State#state.client):put(UpdatedObj) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State}
            end;
        {ok, not_found} ->
            NewObj = [Key, NewValue],
            case (State#state.client):put(NewObj) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State}
            end
    end;

run(delete, KeyGen, _ValueGen, State) ->
    case (State#state.client):delete([KeyGen()]) of
        ok ->
            {ok, State};
        % {error, not_found} ->
        %     {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(proper_delete, KeyGen, _ValueGen, State) ->
    Key = KeyGen(),
    case (State#state.client):get(Key) of
        {ok, {_Values, Context}} ->
            DelExistingObj = [Key, Context],
            case (State#state.client):delete(DelExistingObj) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State}
            end;
        {ok, not_found} ->
            case (State#state.client):delete([Key]) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State}
            end
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

ping_each([]) ->
    ok;
ping_each([Node | Rest]) ->
    case net_adm:ping(Node) of
        pong ->
            ping_each(Rest);
        pang ->
            ?FAIL_MSG("Failed to ping node ~p\n", [Node])
    end.
