%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc xl_digraph test routines
%%
%% @end
%% --------------------------

-module(xl_digraph_tests).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    {setup, fun() -> mnesia:start() end, fun(ok) -> mnesia:stop() end,  
     [
      ?_assertError(badarg, xl_digraph:new([bad_type]))
     ]
    }.


delete_test_() ->
    {setup, fun() -> mnesia:start(), G = xl_digraph:new(), G end, fun(_G) -> mnesia:stop() end,  
     fun(G) -> [?_assertMatch(true,  xl_digraph:delete(G))] end}.


info_test_() ->
    {setup, fun() -> mnesia:start(), G = xl_digraph:new(), G end, fun(_G) -> mnesia:stop() end,  
     fun(G) ->
	     [
	      ?_assertMatch([{cyclicity, cyclic},{memory, _},{protection,protected}],  xl_digraph:info(G))
	      %%?_assertMatch([{cyclicity, cyclic},{memory, _},{protection,protected}],  xl_digraph:info(G))
	     ]
     end}.


add_delete_vertex_test_() ->
    {setup, fun() -> mnesia:start(), G = xl_digraph:new(), G end, fun(_G) -> mnesia:stop() end,  
     fun(G) ->
	     {inorder,
	      [
	       ?_assertMatch(['$v'|0],       xl_digraph:add_vertex(G)),
	       ?_assertMatch({['$v'|0],[]},  xl_digraph:vertex(G, ['$v'|0])),
	       ?_assertMatch([['$v'|0]],     xl_digraph:vertices(G)),
	       ?_assertMatch([['$v'|0]],     xl_digraph:source_vertices(G)),
	       ?_assertMatch([['$v'|0]],     xl_digraph:sink_vertices(G)),
	       ?_assertMatch(['$v'|1],       xl_digraph:add_vertex(G)),
	       ?_assertMatch(['$v'|2],       xl_digraph:add_vertex(G)),
	       ?_assertEqual(3,              xl_digraph:no_vertices(G)),
	       ?_assertMatch("foo",          xl_digraph:add_vertex(G, "foo")),
	       ?_assertMatch({"foo",[]},     xl_digraph:vertex(G, "foo")),
	       ?_assertMatch("bar",          xl_digraph:add_vertex(G, "bar")),
	       ?_assertMatch({"foo",[]},     xl_digraph:vertex(G, "foo")),
	       ?_assertMatch("next",         xl_digraph:add_vertex(G, "next", label)),
	       ?_assertEqual(6,              xl_digraph:no_vertices(G)),	       
	       ?_assertMatch(true,           xl_digraph:del_vertex(G, "next")),
	       ?_assertEqual(5,              xl_digraph:no_vertices(G)),
	       ?_assertMatch(true,           xl_digraph:del_vertices(G, ["foo", "bar", ['$v'|0], ['$v'|1], ['$v'|2]])),
	       ?_assertEqual(0,              xl_digraph:no_vertices(G))
	      ]
	     }
     end}.

vertex_test_() ->
    {setup, 
     fun() -> mnesia:start(),
	      G = xl_digraph:new(),
	      V = xl_digraph:add_vertex(G),
	      {G, V} end,
     fun(_G) -> mnesia:stop() end,
     fun({G, V}) ->
	     {inorder,
	      [
	       ?_assertMatch([['$v'|0]],     xl_digraph:source_vertices(G)),
	       ?_assertMatch([['$v'|0]],     xl_digraph:sink_vertices(G)),
	       ?_assertEqual(1,              xl_digraph:no_vertices(G)),
	       ?_assertEqual(0,              xl_digraph:in_degree(G, V)),
	       ?_assertEqual(0,              xl_digraph:out_degree(G, V))
	      ]
	     }
     end}.



add_delete_edge_test_() ->
    {setup, fun() -> mnesia:start(), G = xl_digraph:new(), G end, fun(_G) -> mnesia:stop() end,  
     fun(G) ->
	     {inorder,
	      [
	       ?_assertMatch("foo",          xl_digraph:add_vertex(G, "foo")),
	       ?_assertMatch("bar",          xl_digraph:add_vertex(G, "bar")),
	       ?_assertMatch("next",         xl_digraph:add_vertex(G, "next")),
	       ?_assertMatch(['$e'|0],         xl_digraph:add_edge(G, "foo", "bar")),
	       ?_assertMatch(['$e'|1],         xl_digraph:add_edge(G, "foo", "next")),
	       ?_assertMatch({error, {bad_vertex, "not_exist"}}, xl_digraph:add_edge(G, "foo", "not_exist"))
	      ]
	     }
     end}.

share_db_test_() ->
    {setup, fun() -> mnesia:start(), G = xl_digraph:new(), G end, fun(_G) -> mnesia:stop() end,  
     fun(G) ->
             Tbls = xl_digraph:info(G, tables),
             G1 = xl_digraph:new([], Tbls),
	     {inorder,
	      [
	       ?_assertMatch("foo",          xl_digraph:add_vertex(G, "foo")),
	       ?_assertMatch("bar",          xl_digraph:add_vertex(G1, "bar")),
	       ?_assertMatch("next",         xl_digraph:add_vertex(G, "next")),
	       ?_assertEqual(3,              xl_digraph:no_vertices(G)),
	       ?_assertEqual(3,              xl_digraph:no_vertices(G1)),
	       ?_assertMatch(['$e'|0],         xl_digraph:add_edge(G1, "foo", "bar")),
	       ?_assertMatch(['$e'|1],         xl_digraph:add_edge(G, "foo", "next")),
	       ?_assertEqual(2,              xl_digraph:no_edges(G)),
	       ?_assertEqual(2,              xl_digraph:no_edges(G1)),
	       ?_assertMatch({error, {bad_vertex, "not_exist"}}, xl_digraph:add_edge(G, "foo", "not_exist"))
	      ]
	     }
     end}.

persistence_test() ->
    mnesia:delete_schema([node()]),
    Opts = [{name, persistence_test},
            {nodes, [node()]},
           {tab_options, [{disc_copies, [node()]}]}],
    G = xl_digraph:new([], Opts),
    ?assertMatch("foo",          xl_digraph:add_vertex(G, "foo")),
    ?assertMatch("bar",          xl_digraph:add_vertex(G, "bar")),
    ?assertMatch("next",         xl_digraph:add_vertex(G, "next")),
    ?assertMatch(['$e'|0],         xl_digraph:add_edge(G, "foo", "bar")),
    ?assertMatch(['$e'|1],         xl_digraph:add_edge(G, "foo", "next")),
    ?assertMatch({error, {bad_vertex, "not_exist"}}, xl_digraph:add_edge(G, "foo", "not_exist")),
    %% stop mnesia
    ?assertMatch(stopped, mnesia:stop()),
    G1 = xl_digraph:new([], Opts),
%    TabList = ['vertices-persistence_test',
%               'edges-persistence_test',
%               'neighbours-persistence_test'],
%    mnesia:wait_for_tables(TabList, 2000),
    ?assertMatch("reload",         xl_digraph:add_vertex(G1, "reload")),
    ?assertEqual(4,              xl_digraph:no_vertices(G1)),
    ?assertMatch(['$e'|_],         xl_digraph:add_edge(G1, "foo", "reload")),
    ?assertEqual(3,              xl_digraph:no_edges(G1)),
    mnesia:stop(), 
    mnesia:delete_schema([node()]).
    


%% %% @doc source_vertices/1
%% %% TODO: add real test, this is a placeholder
%% source_vertices_10_test() ->
%%     ok.

%% %% @doc sink_vertices/1
%% %% TODO: add real test, this is a placeholder
%% sink_vertices_10_test() ->
%%     ok.

%% %% @doc add_edge/3
%% add_edge_10_test() ->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new(),
%%     V1 = xl_digraph:add_vertex(G),
%%     V2 = xl_digraph:add_vertex(G),
%%     ['$e'|0] = xl_digraph:add_edge(G,V1,V2),
%%     stopped = mnesia:stop().    

%% %% @doc add_edge/4
%% add_edge_20_test() ->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new(),
%%     V1 = xl_digraph:add_vertex(G),
%%     V2 = xl_digraph:add_vertex(G),
%%     ['$e'|0] = xl_digraph:add_edge(G,V1,V2,"foo"),
%%     stopped = mnesia:stop().

%% %% @doc del_edge/2
%% %% TODO: check response of call
%% del_edge_10_test() ->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new(),
%%     V1 = xl_digraph:add_vertex(G),
%%     V2 = xl_digraph:add_vertex(G),
%%     E = xl_digraph:add_edge(G,V1,V2),
%%     A = xl_digraph:del_edge(G,E),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().    

%% %% @doc del_edge/2
%% %% TODO: check response of call
%% del_edges_10_test() ->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new(),
%%     V1 = xl_digraph:add_vertex(G),
%%     V2 = xl_digraph:add_vertex(G),
%%     E = xl_digraph:add_edge(G,V1,V2),
%%     A = xl_digraph:del_edges(G,[E]),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().    

%% %% @doc del_path/2
%% %% TODO: check response of call
%% del_path_10_test() ->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new(),
%%     V1 = xl_digraph:add_vertex(G),
%%     V2 = xl_digraph:add_vertex(G),
%%     V3 = xl_digraph:add_vertex(G),
%%     xl_digraph:add_edge(G,V1,V2),
%%     xl_digraph:add_edge(G,V2,V3),
%%     A = xl_digraph:del_path(G,V1,V3),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().    


%% %% @doc in_degree
%% %% TODO: check response of call
%% in_degree_10_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new(),
%%     V = xl_digraph:add_vertex(G),
%%     xl_digraph:in_degree(G,V),
%%     stopped = mnesia:stop().

%% %% @doc in_neighbours
%% %% TODO: check response of call
%% in_neighbours_10_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new([cyclic]),
%%     V = xl_digraph:add_vertex(G),
%%     xl_digraph:in_neighbours(G,V),
%%     stopped = mnesia:stop().

%% %% @doc in_neighbours
%% %% TODO: check response of call
%% in_edges_10_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new([cyclic]),
%%     V = xl_digraph:add_vertex(G),
%%     xl_digraph:in_edges(G,V),
%%     stopped = mnesia:stop().

%% %% @doc out_neighbours
%% %% TODO: check response of call
%% out_degree_10_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new([cyclic]),
%%     V = xl_digraph:add_vertex(G),
%%     xl_digraph:out_degree(G,V),
%%     stopped = mnesia:stop().


%% %% @doc out_neighbours
%% %% TODO: check response of call
%% out_neighbours_10_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new([cyclic]),
%%     V = xl_digraph:add_vertex(G),
%%     xl_digraph:out_neighbours(G,V),
%%     stopped = mnesia:stop().

%% %% @doc out_edges/2
%% %% TODO: check response of call
%% out_edges_10_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new([cyclic]),
%%     V = xl_digraph:add_vertex(G),
%%     xl_digraph:out_edges(G,V),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% no_edges_10_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new([cyclic]),
%%     xl_digraph:no_edges(G),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% edges_10_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new(),
%%     V1 = xl_digraph:add_vertex(G),
%%     V2 = xl_digraph:add_vertex(G),
%%     xl_digraph:add_edge(G, V1, V2),
%%     A = xl_digraph:edges(G),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% edges_20_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new([cyclic]),
%%     V1 = xl_digraph:add_vertex(G),
%%     V2 = xl_digraph:add_vertex(G),
%%     E = xl_digraph:add_edge(G, V1, V2),
%%     xl_digraph:edges(G, E),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% edge_10_test()->
%%     ok = mnesia:start(),
%%     G = xl_digraph:new([cyclic]),
%%     V1 = xl_digraph:add_vertex(G),
%%     V2 = xl_digraph:add_vertex(G),
%%     E = xl_digraph:add_edge(G, V1, V2),
%%     xl_digraph:edge(G, E),
%%     stopped = mnesia:stop().
