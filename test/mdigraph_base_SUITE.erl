%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2012. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(mdigraph_base_SUITE).

-define(STANDALONE,1).

-ifdef(STANDALONE).
-define(line, put(line, ?LINE), ).
-else.
-include_lib("test_server/include/test_server.hrl").
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([opts/1, degree/1, path/1, cycle/1, vertices/1,
	 edges/1, data/1, otp_3522/1, otp_3630/1, otp_8066/1]).

-export([spawn_graph/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() -> [{timetrap,{seconds,30}}].
% [{ct_hooks,[ts_install_cth]}].

all() -> 
    [opts, degree, path, cycle, {group, misc},
     {group, tickets}].

groups() -> 
    [{misc, [], [vertices, edges, data]},
     {tickets, [], [otp_3522, otp_3630, otp_8066]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

opts(doc) -> [];
opts(suite) -> [];
opts(Config) when is_list(Config) ->
    ?line {P1,G1} = spawn_graph([public]),
    ?line x = mdigraph:add_vertex(G1, x),
    ?line kill_graph(P1),
    ?line {P2,G2} = spawn_graph([private]),
    ?line x = mdigraph:add_vertex(G2, x),
    ?line kill_graph(P2),
    ?line {P3,G3} = spawn_graph([protected]),
    ?line x = mdigraph:add_vertex(G3, x),
    ?line kill_graph(P3),
    ?line Template = [{v1,[v2]}, {v2,[v3]}, {v3,[v4]}, {v4,[]}],
    ?line G4 = build_graph([], Template),
    ?line e = mdigraph:add_edge(G4, e, v4, v1, []),
    ?line mdigraph:delete(G4),
    ?line G5 = build_graph([cyclic], Template),
    ?line e = mdigraph:add_edge(G5, e, v4, v1, []),
    ?line mdigraph:delete(G5),
    ?line G6 = build_graph([acyclic], Template),
    ?line acyclic = info(G6, cyclicity),
    ?line {error, {bad_edge,_}} = mdigraph:add_edge(G6, v4, v1),
    ?line mdigraph:delete(G6),
    ok.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

degree(doc) -> [];
degree(suite) -> [];
degree(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x1,[]}, {x2,[x1]}, {x3,[x1,x2]},
			       {x4,[x1,x2,x3]}, {x5,[x1,x2,x3,x4]}]),
    %% out degree
    ?line 0 = mdigraph:out_degree(G, x1),
    ?line 1 = mdigraph:out_degree(G, x2),
    ?line 2 = mdigraph:out_degree(G, x3),
    ?line 3 = mdigraph:out_degree(G, x4),
    ?line 4 = mdigraph:out_degree(G, x5),
    %% out neighbours
    ?line [] = check(mdigraph:out_neighbours(G, x1), []),
    ?line [] = check(mdigraph:out_neighbours(G, x2), [x1]),
    ?line [] = check(mdigraph:out_neighbours(G, x3), [x1,x2]),
    ?line [] = check(mdigraph:out_neighbours(G, x4), [x1,x2,x3]),
    ?line [] = check(mdigraph:out_neighbours(G, x5), [x1,x2,x3,x4]),

    %% in degree
    ?line 4 = mdigraph:in_degree(G, x1),
    ?line 3 = mdigraph:in_degree(G, x2),
    ?line 2 = mdigraph:in_degree(G, x3),
    ?line 1 = mdigraph:in_degree(G, x4),
    ?line 0 = mdigraph:in_degree(G, x5),
    %% in neighbours
    ?line [] = check(mdigraph:in_neighbours(G, x1), [x2,x3,x4,x5]),
    ?line [] = check(mdigraph:in_neighbours(G, x2), [x3,x4,x5]),
    ?line [] = check(mdigraph:in_neighbours(G, x3), [x4,x5]),
    ?line [] = check(mdigraph:in_neighbours(G, x4), [x5]),
    ?line [] = check(mdigraph:in_neighbours(G, x5), []),
    mdigraph:delete(G),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path(doc) -> [];
path(suite) -> [];
path(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x1,[x2,x3]}, {x2,[x4]}, {x3,[x4]},
			       {x4,[x5,x6]}, {x5,[x7]}, {x6,[x7]}]),
    ?line Vi = case mdigraph:get_path(G, x1, x7) of
		   [x1,x2,x4,x5,x7] -> mdigraph:del_vertex(G, x5), x6;
		   [x1,x2,x4,x6,x7] -> mdigraph:del_vertex(G, x6), x5;
		   [x1,x3,x4,x5,x7] -> mdigraph:del_vertex(G, x5), x6;
		   [x1,x3,x4,x6,x7] -> mdigraph:del_vertex(G, x6), x5
	       end,
    ?line Vj = case mdigraph:get_path(G, x1, x7) of
		   [x1,x2,x4,Vi,x7] -> mdigraph:del_vertex(G,x2), x3;
		   [x1,x3,x4,Vi,x7] -> mdigraph:del_vertex(G,x3), x2
	       end,
    ?line [x1,Vj,x4,Vi,x7] = mdigraph:get_path(G, x1, x7),
    ?line mdigraph:del_vertex(G, Vj),
    ?line false = mdigraph:get_path(G, x1, x7),
    ?line [] = check(mdigraph:vertices(G), [x1,x4,Vi,x7]),
    mdigraph:delete(G),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cycle(doc) -> [];
cycle(suite) -> [];
cycle(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x1,[x2,x3]}, {x2,[x4]}, {x3,[x4]},
			       {x4,[x5,x6]}, {x5,[x7]}, {x6,[x7,x8]},
			       {x8,[x3,x8]}]), 
    ?line false = mdigraph:get_cycle(G, x1),
    ?line false = mdigraph:get_cycle(G, x2),
    ?line false = mdigraph:get_cycle(G, x5),
    ?line false = mdigraph:get_cycle(G, x7),
    ?line [x3,x4,x6,x8,x3] = mdigraph:get_cycle(G, x3),
    ?line [x4,x6,x8,x3,x4] = mdigraph:get_cycle(G, x4),
    ?line [x6,x8,x3,x4,x6] = mdigraph:get_cycle(G, x6),
    ?line [x8,x3,x4,x6,x8] = mdigraph:get_cycle(G, x8),
    ?line mdigraph:del_vertex(G, x4),
    ?line [x8] = mdigraph:get_cycle(G, x8),
    mdigraph:delete(G),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



vertices(doc) -> [];
vertices(suite) -> [];
vertices(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x,[]}, {y,[]}]),
    ?line [] = check(mdigraph:vertices(G), [x,y]),
    ?line mdigraph:del_vertices(G, [x,y]),
    ?line [] = mdigraph:vertices(G),
    ?line mdigraph:delete(G),
    ok.

edges(doc) -> [];
edges(suite) -> [];
edges(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x, [{exy,y},{exx,x}]},
			       {y, [{eyx,x}]}
			      ]),
    ?line [] = check(mdigraph:edges(G), [exy, eyx, exx]),
    ?line [] = check(mdigraph:out_edges(G, x), [exy,exx]),
    ?line [] = check(mdigraph:in_edges(G, x), [eyx,exx]),
    ?line [] = check(mdigraph:out_edges(G, y), [eyx]),
    ?line [] = check(mdigraph:in_edges(G, y), [exy]),
    ?line true = mdigraph:del_edges(G, [exy, eyx, does_not_exist]),
    ?line [exx] = mdigraph:edges(G),
    ?line [] = check(mdigraph:out_edges(G, x), [exx]),
    ?line [] = check(mdigraph:in_edges(G, x), [exx]),
    ?line [] = check(mdigraph:out_edges(G, y), []),
    ?line [] = check(mdigraph:in_edges(G, y), []),
    ?line mdigraph:del_vertices(G, [x,y]),
    ?line [] = mdigraph:edges(G),
    ?line [] = mdigraph:vertices(G),
    ?line mdigraph:delete(G),
    ok.

data(doc) -> [];
data(suite) -> [];
data(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x, [{exy, y}]}, {y, []}]),
    
    ?line {x,[]} = mdigraph:vertex(G, x),
    ?line {y,[]} = mdigraph:vertex(G, y),
    ?line {exy,x,y,[]} = mdigraph:edge(G, exy),

    ?line mdigraph:add_edge(G, exy, x, y, {data,x,y}),
    ?line E = mdigraph:add_edge(G, x, y, {data,y,x}),
    ?line mdigraph:add_vertex(G, x, {any}),
    ?line mdigraph:add_vertex(G, y, '_'),

    ?line {x,{any}} = mdigraph:vertex(G, x),
    ?line {y,'_'} = mdigraph:vertex(G, y),
    ?line {exy,x,y,{data,x,y}} = mdigraph:edge(G, exy),
    ?line {E,x,y,{data,y,x}} = mdigraph:edge(G, E),
    ?line true = mdigraph:del_edge(G, E),
    ?line false = mdigraph:edge(G, E),
    ?line true = sane(G),
    ?line mdigraph:delete(G),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



otp_3522(doc) -> [];
otp_3522(suite) -> [];
otp_3522(Config) when is_list(Config) ->
    ?line G1 = build_graph([acyclic], [{x, []}]),
    ?line {error, {bad_edge,_}} = mdigraph:add_edge(G1, x, x),
    ?line true = mdigraph:delete(G1),

    ?line G = mdigraph:new(),
    ?line 0 = mdigraph:no_vertices(G),
    ?line 0 = mdigraph:no_edges(G),
    ?line V1 = mdigraph:add_vertex(G),
    ?line '$vid' = mdigraph:add_vertex(G, '$vid'),
    ?line V2 = mdigraph:add_vertex(G),
    ?line '$eid' = mdigraph:add_edge(G, '$eid', V1, V2, []),
    ?line E = mdigraph:add_edge(G, V1, V2),
    ?line 3 = mdigraph:no_vertices(G),
    ?line 2 = mdigraph:no_edges(G),
    ?line cyclic = info(G, cyclicity),
    ?line protected = info(G, protection),

    ?line [] = check(mdigraph:in_edges(G, V2), ['$eid', E]),
    ?line [] = check(mdigraph:out_edges(G, V1), ['$eid', E]),
    ?line [] = check(mdigraph:vertices(G), [V1,V2,'$vid']),
    ?line [] = check(mdigraph:edges(G), [E, '$eid']),
    ?line true = sane(G),
    ?line true = mdigraph:delete(G),
    ok.

otp_3630(doc) -> [];
otp_3630(suite) -> [];
otp_3630(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x, [{exy,y},{exx,x}]},
			       {y, [{eyy,y},{eyx,x}]}
			      ]),
    ?line [x,y] = mdigraph:get_path(G, x, y),
    ?line [y,x] = mdigraph:get_path(G, y, x),

    ?line [x,x] = mdigraph:get_short_path(G, x, x),
    ?line [y,y] = mdigraph:get_short_path(G, y, y),
    ?line true = mdigraph:delete(G),

    ?line G1 = build_graph([], [{1, [{12,2},{13,3},{11,1}]},
				{2, [{23,3}]},
				{3, [{34,4},{35,5}]},
				{4, [{45,5}]},
				{5, [{56,6},{57,7}]},
				{6, [{67,7}]},
				{7, [{71,1}]}
			       ]),
    
    ?line [1,3,5,7] = mdigraph:get_short_path(G1, 1, 7),
    ?line [3,5,7,1,3] = mdigraph:get_short_cycle(G1, 3),
    ?line [1,1] = mdigraph:get_short_cycle(G1, 1),
    ?line true = mdigraph:delete(G1),

    F = 0.0, I = round(F),
    ?line G2 = mdigraph:new([acyclic]),
    ?line mdigraph:add_vertex(G2, F),
    ?line mdigraph:add_vertex(G2, I),
    ?line E = mdigraph:add_edge(G2, F, I),
    ?line true = not is_tuple(E),
    ?line true = sane(G2),
    ?line true = mdigraph:delete(G2),

    ok.

otp_8066(doc) -> [];
otp_8066(suite) -> [];
otp_8066(Config) when is_list(Config) ->
    fun() ->
            D = mdigraph:new(),
            V1 = mdigraph:add_vertex(D),
            V2 = mdigraph:add_vertex(D),
            _ = mdigraph:add_edge(D, V1, V2),
            ?line [V1, V2] = mdigraph:get_path(D, V1, V2),
            ?line true = sane(D),
            ?line true = mdigraph:del_path(D, V1, V2),
            ?line true = sane(D),
            ?line false = mdigraph:get_path(D, V1, V2),
            ?line true = mdigraph:del_path(D, V1, V2),
            ?line true = mdigraph:delete(D)
    end(),

    fun() ->
            D = mdigraph:new(),
            V1 = mdigraph:add_vertex(D),
            V2 = mdigraph:add_vertex(D),
            _ = mdigraph:add_edge(D, V1, V2),
            _ = mdigraph:add_edge(D, V1, V2),
            _ = mdigraph:add_edge(D, V1, V1),
            _ = mdigraph:add_edge(D, V2, V2),
            ?line [V1, V2] = mdigraph:get_path(D, V1, V2),
            ?line true = sane(D),
            ?line true = mdigraph:del_path(D, V1, V2),
            ?line false = mdigraph:get_short_path(D, V2, V1),

            ?line true = sane(D),
            ?line false = mdigraph:get_path(D, V1, V2),
            ?line true = mdigraph:del_path(D, V1, V2),
            ?line true = mdigraph:delete(D)
    end(),

    fun() ->
            G = mdigraph:new(),
            W1 = mdigraph:add_vertex(G),
            W2 = mdigraph:add_vertex(G),
            W3 = mdigraph:add_vertex(G),
            W4 = mdigraph:add_vertex(G),
            _ = mdigraph:add_edge(G,['$e'|0], W1, W2, {}),
            ?line {error,{bad_vertex, bv}} =
                mdigraph:add_edge(G, edge, bv, W1, {}),
            ?line {error,{bad_vertex, bv}} =
                mdigraph:add_edge(G, edge, W1, bv, {}),
            ?line false = mdigraph:get_short_cycle(G, W1),
            ?line {error, {bad_edge,_}} =
                mdigraph:add_edge(G,['$e'|0], W3, W4, {}),
            ?line true = sane(G),
            ?line true = mdigraph:delete(G)
    end(),
    ok.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sane(G) ->
    sane1(G),
    erase(sane) =:= undefined.

sane1(G) ->
    %% etab: {E, V1, V2, Label}
    %% ntab: {{out,V},E} eller {{in,V},E}
    %% vtab: {V,Label}

    Es = mdigraph:edges(G),
    Vs = mdigraph:vertices(G),
    VEs = lists:flatmap(fun(V) -> mdigraph:edges(G, V) end, Vs),
    case lists:sort(Es++Es) =:= lists:sort(VEs) of
        true -> ok;
        false ->
            io:format("Bad edges~n", []), put(sane, no)
    end,

    lists:foreach(
      fun(E) ->
              Edge = {E, V1, V2, _L} = mdigraph:edge(G, E),
              case {mdigraph:vertex(G, V1), mdigraph:vertex(G, V2)} of
                  {{V1, _}, {V2, _}} -> ok;
                  _ -> io:format("Missing vertex ~p~n", [Edge]), put(sane, no)
              end,
              In = mdigraph:in_edges(G, V2),
              case lists:member(E, In) of 
                  true -> ok;
                  false ->
                      io:format("Missing in-neighbour ~p~n", [Edge]),
                      put(sane, no)
              end,
              Out = mdigraph:out_edges(G, V1),
              case lists:member(E, Out) of 
                  true -> ok;
                  false ->
                      io:format("Missing out-neighbour ~p~n", [Edge]),
                      put(sane, no)
              end
      end, Es),

    lists:foreach(
      fun(V) ->
              InEs = mdigraph:in_edges(G, V),
              %% *All* in-edoges of V
              lists:foreach(
                fun(E) ->
                        case mdigraph:edge(G, E) of
                            {E, _, V, _} -> ok;
                            _ -> 
                                io:format("Bad in-edge ~p: ~p~n", [V, E]),
                                put(sane, no)
                        end
                end, InEs),
              OutEs = mdigraph:out_edges(G, V),
              lists:foreach(
                fun(E) ->
                        case mdigraph:edge(G, E) of
                            {E, V, _, _} -> ok;
                            _ -> 
                                io:format("Bad out-edge ~p: ~p~n", [V, E]),
                                put(sane, no)
                        end
                end, OutEs)
      end, Vs),
    
    InEs = lists:flatmap(fun(V) -> mdigraph:in_edges(G, V) end, Vs),
    OutEs = lists:flatmap(fun(V) -> mdigraph:out_edges(G, V) end, Vs),
    lists:foreach(
      fun(E) ->
              case mdigraph:edge(G, E) of
                  {E, _, _, _} -> ok;
                  _ -> 
                      io:format("Unknown edge (neighbour) ~p~n", [E]),
                      put(sane, no)
              end
      end, InEs++OutEs),

    N_in = length(InEs),
    N_out = length(OutEs),
    N_edges = mdigraph:no_edges(G),
    if 
        N_in =/= N_out ->
            io:format("Number of in- and out-edges differs~n", []),
            put(sane, no);
        N_in+N_out =/= N_edges+N_edges  ->
            io:format("Invalid number of edges (~p+~p =/= 2*~p)~n",
                      [N_in, N_out, N_edges]),
            put(sane, no);
        true -> ok
    end,
    Edges = [mdigraph:edge(G, E) || E <- Es],
    EVs = lists:usort([V || {_, V, _, _} <- Edges] ++
                      [V || {_, _, V, _} <- Edges]),
    lists:foreach(
      fun(V) ->
              case mdigraph:vertex(G, V) of
                  {_, _} -> ok;
                  false ->
                      io:format("Unknown vertex in edge: ~p~n", [V]),
                      put(sane, no)
              end
      end, EVs),

    %% sink_vertices and source_vertices were introduced in 2001. They
    %% are not documented.

    %% sink: a vertex with no outgoing edges
    SinkVs = [V || V <- Vs, mdigraph:out_edges(G, V) =:= [] ],
    case lists:sort(SinkVs) =:=  lists:sort(mdigraph:sink_vertices(G)) of
        true -> ok;
        false -> 
            io:format("Bad sinks~n"), put(sane, no)
    end,
    %% sink: a vertex with no incoming edges
    SourceVs = [V || V <- Vs, mdigraph:in_edges(G, V) =:= [] ],
    case lists:sort(SourceVs) =:=  lists:sort(mdigraph:source_vertices(G)) of
        true -> ok;
        false -> 
            io:format("Bad sources~n"), put(sane, no)
    end,

    true.

build_graph(Opts, Gs) ->
    G = mdigraph:new(Opts),
    build_g(G, Gs).

build_g(G, [{V,Ns} | Gs]) ->
    mdigraph:add_vertex(G, V),
    build_ns(G, V, Ns),
    build_g(G, Gs);
build_g(G, []) -> 
    true = sane(G),
    G.

build_ns(G, V, [{E,W} | Ns]) ->
    mdigraph:add_vertex(G, W),
    mdigraph:add_edge(G, E, V, W, []),
    build_ns(G, V, Ns);
build_ns(G, V, [W | Ns]) ->
    mdigraph:add_vertex(G, W),
    mdigraph:add_edge(G, V, W),
    build_ns(G, V, Ns);
build_ns(_G, _V, []) ->
    true.

%% Spawn a process that create a graph return {Pid, Graph}

spawn_graph(Opts) ->
    Pid = spawn(?MODULE, spawn_graph, [self(),Opts]),
    receive
	{Pid, G} -> {Pid,G}
    end.

%% Create a graph and wait for die message
spawn_graph(Starter, Opts) ->
    G = mdigraph:new(Opts),
    Starter ! {self(), G},
    receive
	die -> true
    end.

info(G, What) ->
    case lists:keysearch(What, 1, mdigraph:info(G)) of
	{value, {What, Value}} -> Value;
	false -> []
    end.

%% Kill process created by spawn_graph
kill_graph(Pid) ->
    Pid ! die.

check(R0, E0) ->
    R = lists:sort(R0),
    E = lists:sort(E0),
    case R of
	E ->
	    [];
	_ ->
	    (R -- E) ++ (E -- R)
    end.
