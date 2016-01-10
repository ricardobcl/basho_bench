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
-module(basho_bench_driver_basic_db).

-export([new/1,
         terminate/2,
         run/4]).

-include("basho_bench.hrl").

-record(state, {client, options}).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    %% Make sure the path is setup such that we can get at basic_db
    case code:which(basic_db) of
        non_existing ->
            ?FAIL_MSG("~s requires basic_db module to be available on code path.\n",
                      [?MODULE]);
        _ ->
            ok
    end,

    Nodes   = basho_bench_config:get(basic_db_nodes),
    Cookie  = basho_bench_config:get(basic_db_cookie, 'basic_db'),
    MyNode  = basho_bench_config:get(basic_db_mynode, [basho_bench, longnames]),
    SyncInterval    = basho_bench_config:get(basic_db_sync_interval, 200),
    ReplicationRate = basho_bench_config:get(basic_db_replication_fail_rate, 0),
    KillRate        = basho_bench_config:get(basic_db_node_kill_rate, 0),

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

    case basic_db:new_client(TargetNode) of
        {ok, Client} ->
            Client:set_sync_interval(SyncInterval),
            Client:set_kill_node_rate(KillRate),
            {ok, #state { client = Client, options=[{repl_fail_ratio, ReplicationRate}]}};
        {error, Reason2} ->
            ?FAIL_MSG("Failed get a basic_db:new_client to ~p: ~p\n", [TargetNode, Reason2])
    end.

run(get, KeyGen, _ValueGen, State) ->
    case (State#state.client):get_at_node(KeyGen()) of
        {not_found, _Context} ->
            {ok, State};
        {ok, _ValuesContext} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(put, KeyGen, ValueGen, State) ->
    Key = KeyGen(),
    Value = ValueGen(),
    case (State#state.client):new_at_node(Key, Value, State#state.options) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(update, KeyGen, ValueGen, State) ->
    Key = KeyGen(),
    Value = ValueGen(),
    Context = case (State#state.client):get_at_node(Key) of
        {ok, {_Values, Ctx}} -> Ctx;
        {not_found, Ctx} -> Ctx
    end,
    case (State#state.client):put_at_node(Key, Value, Context, State#state.options) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(delete, KeyGen, _ValueGen, State) ->
    Key = KeyGen(),
    Context = case (State#state.client):get_at_node(Key) of
        {ok, {_Values, Ctx}} -> Ctx;
        {not_found, Ctx} -> Ctx
    end,
    case (State#state.client):delete_at_node(Key, Context, State#state.options) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

terminate(_, State) ->
    lager:info("Reseting DB options: client -> ~p", [State#state.client]),
    SyncInterval = basho_bench_config:get(basic_db_sync_interval, 200),
    (State#state.client):set_sync_interval(SyncInterval),
    %% stop killing vnodes
    (State#state.client):set_kill_node_rate(0),
    {ok, State}.


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
