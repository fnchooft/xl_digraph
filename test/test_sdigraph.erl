%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc xl_digraph test routines
%%
%% @end
%% --------------------------

-module(test_sdigraph).
-include_lib("eunit/include/eunit.hrl").

delete_test_() ->
    {setup, fun() -> G = sdigraph:new(), G end, fun(_G) -> ok end,  
     fun(G) -> [?_assertMatch(true,  sdigraph:delete(G))] end}.


info_test_() ->
    {setup, fun() -> G = sdigraph:new(), G end, fun(_G) -> ok end,  
     fun(G) ->
	     [
	      ?_assertMatch([{cyclicity, cyclic},{memory, _},{protection,public}],  sdigraph:info(G))
	      %%?_assertMatch([{cyclicity, cyclic},{memory, _},{protection,protected}],  sdigraph:info(G))
	     ]
     end}.


add_delete_vertex_test_() ->
    {setup, fun() -> G = sdigraph:new(), G end, fun(_G) -> ok end,  
     fun(G) ->
	     {inorder,
	      [
	       ?_assertMatch(['$v'|0],       sdigraph:add_vertex(G)),
	       ?_assertMatch({['$v'|0],[]},  sdigraph:vertex(G, ['$v'|0])),
	       ?_assertMatch([['$v'|0]],     sdigraph:vertices(G)),
	       ?_assertMatch([['$v'|0]],     sdigraph:source_vertices(G)),
	       ?_assertMatch([['$v'|0]],     sdigraph:sink_vertices(G)),
	       ?_assertMatch(['$v'|1],       sdigraph:add_vertex(G)),
	       ?_assertMatch(['$v'|2],       sdigraph:add_vertex(G)),
	       ?_assertEqual(3,              sdigraph:no_vertices(G)),
	       ?_assertMatch("foo",          sdigraph:add_vertex(G, "foo")),
	       ?_assertMatch({"foo",[]},     sdigraph:vertex(G, "foo")),
	       ?_assertMatch("bar",          sdigraph:add_vertex(G, "bar")),
	       ?_assertMatch({"foo",[]},     sdigraph:vertex(G, "foo")),
	       ?_assertMatch("next",         sdigraph:add_vertex(G, "next", label)),
	       ?_assertEqual(6,              sdigraph:no_vertices(G)),	       
	       ?_assertMatch(true,           sdigraph:del_vertex(G, "next")),
	       ?_assertEqual(5,              sdigraph:no_vertices(G)),
	       ?_assertMatch(true,           sdigraph:del_vertices(G, ["foo", "bar", ['$v'|0], ['$v'|1], ['$v'|2]])),
	       ?_assertEqual(0,              sdigraph:no_vertices(G))
	      ]
	     }
     end}.

vertex_test_() ->
    {setup, 
     fun() -> G = sdigraph:new(),
	      V = sdigraph:add_vertex(G),
	      {G, V} end,
     fun(_G) -> ok end,
     fun({G, V}) ->
	     {inorder,
	      [
	       ?_assertMatch([['$v'|0]],     sdigraph:source_vertices(G)),
	       ?_assertMatch([['$v'|0]],     sdigraph:sink_vertices(G)),
	       ?_assertEqual(1,              sdigraph:no_vertices(G)),
	       ?_assertEqual(0,              sdigraph:in_degree(G, V)),
	       ?_assertEqual(0,              sdigraph:out_degree(G, V))
	      ]
	     }
     end}.



add_delete_edge_test_() ->
    {setup, fun() -> G = sdigraph:new(), G end, fun(_G) -> ok end,  
     fun(G) ->
	     {inorder,
	      [
	       ?_assertMatch("foo",          sdigraph:add_vertex(G, "foo")),
	       ?_assertMatch("bar",          sdigraph:add_vertex(G, "bar")),
	       ?_assertMatch("next",         sdigraph:add_vertex(G, "next")),
	       ?_assertMatch(['$e'|0],         sdigraph:add_edge(G, "foo", "bar")),
	       ?_assertMatch(['$e'|1],         sdigraph:add_edge(G, "foo", "next")),
	       ?_assertMatch({error, {bad_vertex, "not_exist"}}, sdigraph:add_edge(G, "foo", "not_exist"))
	      ]
	     }
     end}.

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
%%     G = sdigraph:new(),
%%     V1 = sdigraph:add_vertex(G),
%%     V2 = sdigraph:add_vertex(G),
%%     ['$e'|0] = sdigraph:add_edge(G,V1,V2),
%%     stopped = mnesia:stop().    

%% %% @doc add_edge/4
%% add_edge_20_test() ->
%%     ok = mnesia:start(),
%%     G = sdigraph:new(),
%%     V1 = sdigraph:add_vertex(G),
%%     V2 = sdigraph:add_vertex(G),
%%     ['$e'|0] = sdigraph:add_edge(G,V1,V2,"foo"),
%%     stopped = mnesia:stop().

%% %% @doc del_edge/2
%% %% TODO: check response of call
%% del_edge_10_test() ->
%%     ok = mnesia:start(),
%%     G = sdigraph:new(),
%%     V1 = sdigraph:add_vertex(G),
%%     V2 = sdigraph:add_vertex(G),
%%     E = sdigraph:add_edge(G,V1,V2),
%%     A = sdigraph:del_edge(G,E),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().    

%% %% @doc del_edge/2
%% %% TODO: check response of call
%% del_edges_10_test() ->
%%     ok = mnesia:start(),
%%     G = sdigraph:new(),
%%     V1 = sdigraph:add_vertex(G),
%%     V2 = sdigraph:add_vertex(G),
%%     E = sdigraph:add_edge(G,V1,V2),
%%     A = sdigraph:del_edges(G,[E]),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().    

%% %% @doc del_path/2
%% %% TODO: check response of call
%% del_path_10_test() ->
%%     ok = mnesia:start(),
%%     G = sdigraph:new(),
%%     V1 = sdigraph:add_vertex(G),
%%     V2 = sdigraph:add_vertex(G),
%%     V3 = sdigraph:add_vertex(G),
%%     sdigraph:add_edge(G,V1,V2),
%%     sdigraph:add_edge(G,V2,V3),
%%     A = sdigraph:del_path(G,V1,V3),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().    


%% %% @doc in_degree
%% %% TODO: check response of call
%% in_degree_10_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new(),
%%     V = sdigraph:add_vertex(G),
%%     sdigraph:in_degree(G,V),
%%     stopped = mnesia:stop().

%% %% @doc in_neighbours
%% %% TODO: check response of call
%% in_neighbours_10_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new([cyclic]),
%%     V = sdigraph:add_vertex(G),
%%     sdigraph:in_neighbours(G,V),
%%     stopped = mnesia:stop().

%% %% @doc in_neighbours
%% %% TODO: check response of call
%% in_edges_10_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new([cyclic]),
%%     V = sdigraph:add_vertex(G),
%%     sdigraph:in_edges(G,V),
%%     stopped = mnesia:stop().

%% %% @doc out_neighbours
%% %% TODO: check response of call
%% out_degree_10_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new([cyclic]),
%%     V = sdigraph:add_vertex(G),
%%     sdigraph:out_degree(G,V),
%%     stopped = mnesia:stop().


%% %% @doc out_neighbours
%% %% TODO: check response of call
%% out_neighbours_10_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new([cyclic]),
%%     V = sdigraph:add_vertex(G),
%%     sdigraph:out_neighbours(G,V),
%%     stopped = mnesia:stop().

%% %% @doc out_edges/2
%% %% TODO: check response of call
%% out_edges_10_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new([cyclic]),
%%     V = sdigraph:add_vertex(G),
%%     sdigraph:out_edges(G,V),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% no_edges_10_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new([cyclic]),
%%     sdigraph:no_edges(G),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% edges_10_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new(),
%%     V1 = sdigraph:add_vertex(G),
%%     V2 = sdigraph:add_vertex(G),
%%     sdigraph:add_edge(G, V1, V2),
%%     A = sdigraph:edges(G),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% edges_20_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new([cyclic]),
%%     V1 = sdigraph:add_vertex(G),
%%     V2 = sdigraph:add_vertex(G),
%%     E = sdigraph:add_edge(G, V1, V2),
%%     sdigraph:edges(G, E),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% edge_10_test()->
%%     ok = mnesia:start(),
%%     G = sdigraph:new([cyclic]),
%%     V1 = sdigraph:add_vertex(G),
%%     V2 = sdigraph:add_vertex(G),
%%     E = sdigraph:add_edge(G, V1, V2),
%%     sdigraph:edge(G, E),
%%     stopped = mnesia:stop().
