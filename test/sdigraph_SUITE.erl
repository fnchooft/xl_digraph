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
-module(sdigraph_SUITE).

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
    %% OTP-5985: the 'public' option has been removed
%    ?line {'EXIT',{badarg,_}} = (catch sdigraph:new([public])),
    ?line {P1,G1} = spawn_graph([public]),
    ?line x = sdigraph:add_vertex(G1, x),
    ?line kill_graph(P1),
    ?line {P2,G2} = spawn_graph([private]),
    ?line {'EXIT',{badarg,_}} = (catch sdigraph:add_vertex(G2, x)),
    ?line kill_graph(P2),
    ?line {P3,G3} = spawn_graph([protected]),
    ?line {'EXIT',{badarg,_}} = (catch sdigraph:add_vertex(G3, x)),
    ?line kill_graph(P3),
    ?line Template = [{v1,[v2]}, {v2,[v3]}, {v3,[v4]}, {v4,[]}],
    ?line G4 = build_graph([], Template),
    ?line e = sdigraph:add_edge(G4, e, v4, v1, []),
    ?line sdigraph:delete(G4),
    ?line G5 = build_graph([cyclic], Template),
    ?line e = sdigraph:add_edge(G5, e, v4, v1, []),
    ?line sdigraph:delete(G5),
    ?line G6 = build_graph([acyclic], Template),
    ?line acyclic = info(G6, cyclicity),
    ?line {error, {bad_edge,_}} = sdigraph:add_edge(G6, v4, v1),
    ?line sdigraph:delete(G6),
    ok.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

degree(doc) -> [];
degree(suite) -> [];
degree(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x1,[]}, {x2,[x1]}, {x3,[x1,x2]},
			       {x4,[x1,x2,x3]}, {x5,[x1,x2,x3,x4]}]),
    %% out degree
    ?line 0 = sdigraph:out_degree(G, x1),
    ?line 1 = sdigraph:out_degree(G, x2),
    ?line 2 = sdigraph:out_degree(G, x3),
    ?line 3 = sdigraph:out_degree(G, x4),
    ?line 4 = sdigraph:out_degree(G, x5),
    %% out neighbours
    ?line [] = check(sdigraph:out_neighbours(G, x1), []),
    ?line [] = check(sdigraph:out_neighbours(G, x2), [x1]),
    ?line [] = check(sdigraph:out_neighbours(G, x3), [x1,x2]),
    ?line [] = check(sdigraph:out_neighbours(G, x4), [x1,x2,x3]),
    ?line [] = check(sdigraph:out_neighbours(G, x5), [x1,x2,x3,x4]),

    %% in degree
    ?line 4 = sdigraph:in_degree(G, x1),
    ?line 3 = sdigraph:in_degree(G, x2),
    ?line 2 = sdigraph:in_degree(G, x3),
    ?line 1 = sdigraph:in_degree(G, x4),
    ?line 0 = sdigraph:in_degree(G, x5),
    %% in neighbours
    ?line [] = check(sdigraph:in_neighbours(G, x1), [x2,x3,x4,x5]),
    ?line [] = check(sdigraph:in_neighbours(G, x2), [x3,x4,x5]),
    ?line [] = check(sdigraph:in_neighbours(G, x3), [x4,x5]),
    ?line [] = check(sdigraph:in_neighbours(G, x4), [x5]),
    ?line [] = check(sdigraph:in_neighbours(G, x5), []),
    sdigraph:delete(G),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path(doc) -> [];
path(suite) -> [];
path(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x1,[x2,x3]}, {x2,[x4]}, {x3,[x4]},
			       {x4,[x5,x6]}, {x5,[x7]}, {x6,[x7]}]),
    ?line Vi = case sdigraph:get_path(G, x1, x7) of
		   [x1,x2,x4,x5,x7] -> sdigraph:del_vertex(G, x5), x6;
		   [x1,x2,x4,x6,x7] -> sdigraph:del_vertex(G, x6), x5;
		   [x1,x3,x4,x5,x7] -> sdigraph:del_vertex(G, x5), x6;
		   [x1,x3,x4,x6,x7] -> sdigraph:del_vertex(G, x6), x5
	       end,
    ?line Vj = case sdigraph:get_path(G, x1, x7) of
		   [x1,x2,x4,Vi,x7] -> sdigraph:del_vertex(G,x2), x3;
		   [x1,x3,x4,Vi,x7] -> sdigraph:del_vertex(G,x3), x2
	       end,
    ?line [x1,Vj,x4,Vi,x7] = sdigraph:get_path(G, x1, x7),
    ?line sdigraph:del_vertex(G, Vj),
    ?line false = sdigraph:get_path(G, x1, x7),
    ?line [] = check(sdigraph:vertices(G), [x1,x4,Vi,x7]),
    sdigraph:delete(G),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cycle(doc) -> [];
cycle(suite) -> [];
cycle(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x1,[x2,x3]}, {x2,[x4]}, {x3,[x4]},
			       {x4,[x5,x6]}, {x5,[x7]}, {x6,[x7,x8]},
			       {x8,[x3,x8]}]), 
    ?line false = sdigraph:get_cycle(G, x1),
    ?line false = sdigraph:get_cycle(G, x2),
    ?line false = sdigraph:get_cycle(G, x5),
    ?line false = sdigraph:get_cycle(G, x7),
    ?line [x3,x4,x6,x8,x3] = sdigraph:get_cycle(G, x3),
    ?line [x4,x6,x8,x3,x4] = sdigraph:get_cycle(G, x4),
    ?line [x6,x8,x3,x4,x6] = sdigraph:get_cycle(G, x6),
    ?line [x8,x3,x4,x6,x8] = sdigraph:get_cycle(G, x8),
    ?line sdigraph:del_vertex(G, x4),
    ?line [x8] = sdigraph:get_cycle(G, x8),
    sdigraph:delete(G),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



vertices(doc) -> [];
vertices(suite) -> [];
vertices(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x,[]}, {y,[]}]),
    ?line [] = check(sdigraph:vertices(G), [x,y]),
    ?line sdigraph:del_vertices(G, [x,y]),
    ?line [] = sdigraph:vertices(G),
    ?line sdigraph:delete(G),
    ok.

edges(doc) -> [];
edges(suite) -> [];
edges(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x, [{exy,y},{exx,x}]},
			       {y, [{eyx,x}]}
			      ]),
    ?line [] = check(sdigraph:edges(G), [exy, eyx, exx]),
    ?line [] = check(sdigraph:out_edges(G, x), [exy,exx]),
    ?line [] = check(sdigraph:in_edges(G, x), [eyx,exx]),
    ?line [] = check(sdigraph:out_edges(G, y), [eyx]),
    ?line [] = check(sdigraph:in_edges(G, y), [exy]),
    ?line true = sdigraph:del_edges(G, [exy, eyx, does_not_exist]),
    ?line [exx] = sdigraph:edges(G),
    ?line [] = check(sdigraph:out_edges(G, x), [exx]),
    ?line [] = check(sdigraph:in_edges(G, x), [exx]),
    ?line [] = check(sdigraph:out_edges(G, y), []),
    ?line [] = check(sdigraph:in_edges(G, y), []),
    ?line sdigraph:del_vertices(G, [x,y]),
    ?line [] = sdigraph:edges(G),
    ?line [] = sdigraph:vertices(G),
    ?line sdigraph:delete(G),
    ok.

data(doc) -> [];
data(suite) -> [];
data(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x, [{exy, y}]}, {y, []}]),
    
    ?line {x,[]} = sdigraph:vertex(G, x),
    ?line {y,[]} = sdigraph:vertex(G, y),
    ?line {exy,x,y,[]} = sdigraph:edge(G, exy),

    ?line sdigraph:add_edge(G, exy, x, y, {data,x,y}),
    ?line E = sdigraph:add_edge(G, x, y, {data,y,x}),
    ?line sdigraph:add_vertex(G, x, {any}),
    ?line sdigraph:add_vertex(G, y, '_'),

    ?line {x,{any}} = sdigraph:vertex(G, x),
    ?line {y,'_'} = sdigraph:vertex(G, y),
    ?line {exy,x,y,{data,x,y}} = sdigraph:edge(G, exy),
    ?line {E,x,y,{data,y,x}} = sdigraph:edge(G, E),
    ?line true = sdigraph:del_edge(G, E),
    ?line false = sdigraph:edge(G, E),
    ?line true = sane(G),
    ?line sdigraph:delete(G),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



otp_3522(doc) -> [];
otp_3522(suite) -> [];
otp_3522(Config) when is_list(Config) ->
    ?line G1 = build_graph([acyclic], [{x, []}]),
    ?line {error, {bad_edge,_}} = sdigraph:add_edge(G1, x, x),
    ?line true = sdigraph:delete(G1),

    ?line G = sdigraph:new(),
    ?line 0 = sdigraph:no_vertices(G),
    ?line 0 = sdigraph:no_edges(G),
    ?line V1 = sdigraph:add_vertex(G),
    ?line '$vid' = sdigraph:add_vertex(G, '$vid'),
    ?line V2 = sdigraph:add_vertex(G),
    ?line '$eid' = sdigraph:add_edge(G, '$eid', V1, V2, []),
    ?line E = sdigraph:add_edge(G, V1, V2),
    ?line 3 = sdigraph:no_vertices(G),
    ?line 2 = sdigraph:no_edges(G),
    ?line cyclic = info(G, cyclicity),
    ?line public = info(G, protection),

    ?line [] = check(sdigraph:in_edges(G, V2), ['$eid', E]),
    ?line [] = check(sdigraph:out_edges(G, V1), ['$eid', E]),
    ?line [] = check(sdigraph:vertices(G), [V1,V2,'$vid']),
    ?line [] = check(sdigraph:edges(G), [E, '$eid']),
    ?line true = sane(G),
    ?line true = sdigraph:delete(G),
    ok.

otp_3630(doc) -> [];
otp_3630(suite) -> [];
otp_3630(Config) when is_list(Config) ->
    ?line G = build_graph([], [{x, [{exy,y},{exx,x}]},
			       {y, [{eyy,y},{eyx,x}]}
			      ]),
    ?line [x,y] = sdigraph:get_path(G, x, y),
    ?line [y,x] = sdigraph:get_path(G, y, x),

    ?line [x,x] = sdigraph:get_short_path(G, x, x),
    ?line [y,y] = sdigraph:get_short_path(G, y, y),
    ?line true = sdigraph:delete(G),

    ?line G1 = build_graph([], [{1, [{12,2},{13,3},{11,1}]},
				{2, [{23,3}]},
				{3, [{34,4},{35,5}]},
				{4, [{45,5}]},
				{5, [{56,6},{57,7}]},
				{6, [{67,7}]},
				{7, [{71,1}]}
			       ]),
    
    ?line [1,3,5,7] = sdigraph:get_short_path(G1, 1, 7),
    ?line [3,5,7,1,3] = sdigraph:get_short_cycle(G1, 3),
    ?line [1,1] = sdigraph:get_short_cycle(G1, 1),
    ?line true = sdigraph:delete(G1),

    F = 0.0, I = round(F),
    ?line G2 = sdigraph:new([acyclic]),
    ?line sdigraph:add_vertex(G2, F),
    ?line sdigraph:add_vertex(G2, I),
    ?line E = sdigraph:add_edge(G2, F, I),
    ?line true = not is_tuple(E),
    ?line true = sane(G2),
    ?line true = sdigraph:delete(G2),

    ok.

otp_8066(doc) -> [];
otp_8066(suite) -> [];
otp_8066(Config) when is_list(Config) ->
    fun() ->
            D = sdigraph:new(),
            V1 = sdigraph:add_vertex(D),
            V2 = sdigraph:add_vertex(D),
            _ = sdigraph:add_edge(D, V1, V2),
            ?line [V1, V2] = sdigraph:get_path(D, V1, V2),
            ?line true = sane(D),
            ?line true = sdigraph:del_path(D, V1, V2),
            ?line true = sane(D),
            ?line false = sdigraph:get_path(D, V1, V2),
            ?line true = sdigraph:del_path(D, V1, V2),
            ?line true = sdigraph:delete(D)
    end(),

    fun() ->
            D = sdigraph:new(),
            V1 = sdigraph:add_vertex(D),
            V2 = sdigraph:add_vertex(D),
            _ = sdigraph:add_edge(D, V1, V2),
            _ = sdigraph:add_edge(D, V1, V2),
            _ = sdigraph:add_edge(D, V1, V1),
            _ = sdigraph:add_edge(D, V2, V2),
            ?line [V1, V2] = sdigraph:get_path(D, V1, V2),
            ?line true = sane(D),
            ?line true = sdigraph:del_path(D, V1, V2),
            ?line false = sdigraph:get_short_path(D, V2, V1),

            ?line true = sane(D),
            ?line false = sdigraph:get_path(D, V1, V2),
            ?line true = sdigraph:del_path(D, V1, V2),
            ?line true = sdigraph:delete(D)
    end(),

    fun() ->
            G = sdigraph:new(),
            W1 = sdigraph:add_vertex(G),
            W2 = sdigraph:add_vertex(G),
            W3 = sdigraph:add_vertex(G),
            W4 = sdigraph:add_vertex(G),
            _ = sdigraph:add_edge(G,['$e'|0], W1, W2, {}),
            ?line {error,{bad_vertex, bv}} =
                sdigraph:add_edge(G, edge, bv, W1, {}),
            ?line {error,{bad_vertex, bv}} =
                sdigraph:add_edge(G, edge, W1, bv, {}),
            ?line false = sdigraph:get_short_cycle(G, W1),
            ?line {error, {bad_edge,_}} =
                sdigraph:add_edge(G,['$e'|0], W3, W4, {}),
            ?line true = sane(G),
            ?line true = sdigraph:delete(G)
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

    Es = sdigraph:edges(G),
    Vs = sdigraph:vertices(G),
    VEs = lists:flatmap(fun(V) -> sdigraph:edges(G, V) end, Vs),
    case lists:sort(Es++Es) =:= lists:sort(VEs) of
        true -> ok;
        false ->
            io:format("Bad edges~n", []), put(sane, no)
    end,

    lists:foreach(
      fun(E) ->
              Edge = {E, V1, V2, _L} = sdigraph:edge(G, E),
              case {sdigraph:vertex(G, V1), sdigraph:vertex(G, V2)} of
                  {{V1, _}, {V2, _}} -> ok;
                  _ -> io:format("Missing vertex ~p~n", [Edge]), put(sane, no)
              end,
              In = sdigraph:in_edges(G, V2),
              case lists:member(E, In) of 
                  true -> ok;
                  false ->
                      io:format("Missing in-neighbour ~p~n", [Edge]),
                      put(sane, no)
              end,
              Out = sdigraph:out_edges(G, V1),
              case lists:member(E, Out) of 
                  true -> ok;
                  false ->
                      io:format("Missing out-neighbour ~p~n", [Edge]),
                      put(sane, no)
              end
      end, Es),

    lists:foreach(
      fun(V) ->
              InEs = sdigraph:in_edges(G, V),
              %% *All* in-edoges of V
              lists:foreach(
                fun(E) ->
                        case sdigraph:edge(G, E) of
                            {E, _, V, _} -> ok;
                            _ -> 
                                io:format("Bad in-edge ~p: ~p~n", [V, E]),
                                put(sane, no)
                        end
                end, InEs),
              OutEs = sdigraph:out_edges(G, V),
              lists:foreach(
                fun(E) ->
                        case sdigraph:edge(G, E) of
                            {E, V, _, _} -> ok;
                            _ -> 
                                io:format("Bad out-edge ~p: ~p~n", [V, E]),
                                put(sane, no)
                        end
                end, OutEs)
      end, Vs),
    
    InEs = lists:flatmap(fun(V) -> sdigraph:in_edges(G, V) end, Vs),
    OutEs = lists:flatmap(fun(V) -> sdigraph:out_edges(G, V) end, Vs),
    lists:foreach(
      fun(E) ->
              case sdigraph:edge(G, E) of
                  {E, _, _, _} -> ok;
                  _ -> 
                      io:format("Unknown edge (neighbour) ~p~n", [E]),
                      put(sane, no)
              end
      end, InEs++OutEs),

    N_in = length(InEs),
    N_out = length(OutEs),
    N_edges = sdigraph:no_edges(G),
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
    Edges = [sdigraph:edge(G, E) || E <- Es],
    EVs = lists:usort([V || {_, V, _, _} <- Edges] ++
                      [V || {_, _, V, _} <- Edges]),
    lists:foreach(
      fun(V) ->
              case sdigraph:vertex(G, V) of
                  {_, _} -> ok;
                  false ->
                      io:format("Unknown vertex in edge: ~p~n", [V]),
                      put(sane, no)
              end
      end, EVs),

    %% sink_vertices and source_vertices were introduced in 2001. They
    %% are not documented.

    %% sink: a vertex with no outgoing edges
    SinkVs = [V || V <- Vs, sdigraph:out_edges(G, V) =:= [] ],
    case lists:sort(SinkVs) =:=  lists:sort(sdigraph:sink_vertices(G)) of
        true -> ok;
        false -> 
            io:format("Bad sinks~n"), put(sane, no)
    end,
    %% sink: a vertex with no incoming edges
    SourceVs = [V || V <- Vs, sdigraph:in_edges(G, V) =:= [] ],
    case lists:sort(SourceVs) =:=  lists:sort(sdigraph:source_vertices(G)) of
        true -> ok;
        false -> 
            io:format("Bad sources~n"), put(sane, no)
    end,

    true.

build_graph(Opts, Gs) ->
    G = sdigraph:new(Opts),
    build_g(G, Gs).

build_g(G, [{V,Ns} | Gs]) ->
    sdigraph:add_vertex(G, V),
    build_ns(G, V, Ns),
    build_g(G, Gs);
build_g(G, []) -> 
    true = sane(G),
    G.

build_ns(G, V, [{E,W} | Ns]) ->
    sdigraph:add_vertex(G, W),
    sdigraph:add_edge(G, E, V, W, []),
    build_ns(G, V, Ns);
build_ns(G, V, [W | Ns]) ->
    sdigraph:add_vertex(G, W),
    sdigraph:add_edge(G, V, W),
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
    G = sdigraph:new(Opts),
    Starter ! {self(), G},
    receive
	die -> true
    end.

info(G, What) ->
    case lists:keysearch(What, 1, sdigraph:info(G)) of
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
