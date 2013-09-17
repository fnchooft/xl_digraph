%%%-------------------------------------------------------------------
%%% @author Gary Hai <gary@XL59.com>
%%% @copyright (C) 2013, Gary Hai
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2013 by Gary Hai <gary@XL59.com>
%%%-------------------------------------------------------------------
-module(test_so_digraph).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    G1= so_digraph:create([]),
    ?assert({ok, 0} == so_digraph:get(G1, count)),
    ?assert(ok == so_digraph:delete(G1, all)),
%    ?assertError({ok, 0} == so_digraph:get(G1, count)),
    ?assert({error, undefined} == so_digraph:get(G1, count)),

    G0 = sdigraph:new(),
    sdigraph:add_vertex(G0, test, true),
    G2 = so_digraph:create([{graph, G0}]),
    ?assert({ok, 1} == so_digraph:get(G2, count)),
    ?assert({ok, test} == so_digraph:get(G2, [test], id)),
    ?assert({ok, true} == so_digraph:get(G2, [test])),
    ?assert(ok == so_digraph:delete(G2, all)),
    
    G3= so_digraph:create([{home, [root]}]),
    ?assert({ok, 0} == so_digraph:get(G3, count)),
    ?assert({ok, root} == so_digraph:put(G3, [], test, [])),
    ?assert({ok, test} == so_digraph:get(G3, [])),
    ok = so_digraph:delete(G3, []).
    
main_test() ->
    G = so_digraph:create([{module, sdigraph}, {default_link, []}]),
    %%---- new test
    %%--- global access, without home
    {ok, X} = so_digraph:new(G, x),  % generate id
    {ok, Y} = so_digraph:new(G, <<"y">>, y),
    {ok, Z} = so_digraph:new(G, <<"z">>, z),
    {ok, _} = so_digraph:new(G, <<"a">>, a),
    {ok, _} = so_digraph:new(G, <<"b">>, b),
    {ok, AB} = so_digraph:new(G, <<"a/b">>, ab),  % relative
    {ok, ABC} = so_digraph:new(G, <<"/a/b/c">>, abc),  % absolute
    %% link X to Y with name x_y as fatal link
    {ok, _} = so_digraph:new(G, [X, <<"x_y">>], Y, fatal_link),
    {ok, _} = so_digraph:new(G, Y, Z, link),  % link Y to Z with no edge label special
    {ok, _} = so_digraph:new(G, <<"a/c">>, <<"a/b/c">>, link), % link a to a/b/c by name c.
    {error, already_exists} = so_digraph:new(G, <<"a/c">>, <<"a/b/c">>, link),
    {error, already_exists} = so_digraph:new(G, [X, <<"x_y">>], Y, link),
    {ok, Y2Z} = so_digraph:new(G, Y, Z, fatal_link),  % can link Y to Z with different name
    {error, already_exists} = so_digraph:new(G, <<"/a/b">>, ab),

    %%--- local access, with home
    {ok, _, Gb} = so_digraph:new(G, home, <<"/a/b">>, id),
    {ok, ab} = so_digraph:get(Gb, <<>>),
    {ok, b} = so_digraph:get(G, <<"b">>),
    {error,unreachable} = so_digraph:new(Gb, <<"a/y/x">>, nopath),
    {error, already_exists} = so_digraph:new(Gb, <<"c">>, [<<>>, ABC], link),
    {error, already_exists} = so_digraph:new(Gb, <<"c">>, <<"/a/b/c">>, link),
    {ok, Name} = so_digraph:new(Gb, <<"name">>, "/a/b"),
    {ok, Firstname} = so_digraph:new(Gb, <<"name/first">>, "a"),
    {ok, _} = so_digraph:new(Gb, <<"firstname">>, <<"name/first">>, fatal_link),
    {ok, "a"} = so_digraph:get(Gb, <<"firstname">>),

    %%---- get test
    %%--- special key: global
    {ok, 9} = so_digraph:get(G, count),
    {ok, 9} = so_digraph:get(G, count, link),
    {ok, AllLink} = so_digraph:get(G, all, link),
    {ok, AllId} = so_digraph:get(G, all, id),
    {ok, AllObj} = so_digraph:get(G, all),
    FnLink = {{AB, <<"firstname">>}, AB, Firstname, [fatal]},
    {ok, FnLink} = so_digraph:get(G, <<"/a/b/firstname">>, link),
    {ok, FnLink} = so_digraph:get(G, [{AB, <<"firstname">>}], link),
    {error, undefined} = so_digraph:get(G, <<"not_exists">>, link),

    true = lists:member({<<"a">>, <<"b">>}, AllLink),
    true = lists:member({<<"a">>, <<"c">>}, AllLink),
    true = lists:member({AB, <<"name">>}, AllLink),
    true = lists:member({AB, <<"firstname">>}, AllLink),
    true = lists:member(AB, AllId),
    true = lists:member(<<"a">>, AllId),
    true = lists:member(<<"b">>, AllId),
    true = lists:member(<<"z">>, AllId),
    true = lists:member({<<"a">>, a}, AllObj),
    true = lists:member({AB, ab}, AllObj),
    true = lists:member({X, x}, AllObj),
    true = lists:member({Y, y}, AllObj),

    %%--- local
    {ok, AB} = so_digraph:get(Gb, <<>>, id),
    {ok, FnLink} = so_digraph:get(Gb, <<"firstname">>, link),
    {error, undefined} = so_digraph:get(Gb, <<"nothere">>),
    {error, undefined} = so_digraph:get(Gb, <<"/nothere">>),
    {ok, abc} = so_digraph:get(Gb, <<"/a/b/c">>),  % absolute addressing
    {ok, abc} = so_digraph:get(Gb, <<"c">>),  % relative addressing
    
    {ok, 1} = so_digraph:get(Gb, in_degree),
    {ok, 3} = so_digraph:get(Gb, out_degree),
    {ok, 4} = so_digraph:get(Gb, count),
    {ok, Links} = so_digraph:get(Gb, all, link),
    {ok, FatalLinks} = so_digraph:get(Gb, all, fatal_link),
    {ok, Ids} = so_digraph:get(Gb, all, id),
    {ok, InIds} = so_digraph:get(Gb, in, id),
    {ok, InLinks} = so_digraph:get(Gb, in, link),
    {ok, OutIds} = so_digraph:get(Gb, out, id),
    {ok, OutLinks} = so_digraph:get(Gb, out, link),

    true = lists:member({AB, <<"name">>}, Links),
    true = lists:member({AB, <<"firstname">>}, FatalLinks),
    true = lists:member(Name, Ids),
    true = lists:member(<<"a">>, InIds),
    true = lists:member({<<"a">>,<<"b">>}, InLinks),
    true = lists:member(Firstname, OutIds),
    true = lists:member({AB, <<"name">>}, OutLinks),
    
    %%---- put test
    %%--- link update
    %% update link flags directly
    false = lists:member({AB, <<"name">>}, FatalLinks),
    {ok, _} = so_digraph:put(G, [{AB, <<"name">>}], [fatal], link),
    {ok, Fl} = so_digraph:get(Gb, all, fatal_link),
    true = lists:member({AB, <<"name">>}, Fl),
    {error, undefined} = so_digraph:put(G, <<"unknown">>, nothing, link),
    {error, undefined} = so_digraph:put(Gb, <<"unknown">>, nothing, link),
    {ok, _} = so_digraph:put(Gb, <<"name">>, [], link),
    {ok, Fl1} = so_digraph:get(Gb, all, fatal_link),
    false = lists:member({AB, <<"name">>}, Fl1),
    %% update fatal flag only
    {ok, _} = so_digraph:put(Gb, <<"name">>, true, fatal_link),
    {ok, Fl} = so_digraph:get(Gb, all, fatal_link),  % already tested
    {error, undefined} = so_digraph:put(Gb, <<"unknown">>, true, fatal_link),
    %% --- value update
    {ok, _} = so_digraph:put(G, <<"a">>, a1),
    {ok, a1} = so_digraph:get(G, <<"a">>),
    {ok, "a"} = so_digraph:get(Gb, <<"firstname">>),
    {ok, _} = so_digraph:put(G, <<"/a/b/firstname">>, a2),
    {ok, a2} = so_digraph:get(G, <<"/a/b/name/first">>),
    {ok, _} = so_digraph:put(Gb, <<>>, "AB"),  % change the home value
    {ok, "AB"} = so_digraph:get(Gb, []),
    {error, undefined} = so_digraph:put(Gb, <<"unknown">>, nothing),
    {ok, _, Gname} = so_digraph:put(Gb, home, <<"name">>, []),
    {ok, a2} = so_digraph:get(Gname, <<"first">>),
    {ok, _, Gnothere} = so_digraph:put(G, home, <<"nothere">>, []),
    %% nothere is not existed in the graph
    {error, undefined} = so_digraph:get(Gnothere, []),
    {error, undefined} = so_digraph:put(Gnothere, <<"hello">>, world),
    %% put or new operation may create it.
    {ok, _} = so_digraph:put(Gnothere, [], anything),
    {ok, anything} = so_digraph:get(Gnothere, []),
    {ok, _} = so_digraph:new(Gnothere, <<"hello">>, world),
    {ok, world} = so_digraph:get(Gnothere, <<"hello">>), 
    %% cascading location is forbidden for new home
    {error, undefined} = so_digraph:put(G, home, <<"not/here">>, []),
    
    %%---- delete test
    %%--- link deletion
    %% delete link directly by link id.
    {ok, abc} = so_digraph:get(G, <<"/a/c">>),
    ok = so_digraph:delete(G, <<"a/c">>, link),    
    {error, undefined} = so_digraph:get(G, <<"/a/c">>),
    {ok, a2} = so_digraph:get(Gb, <<"name/first">>),
    ok = so_digraph:delete(Gb, <<"name/first">>, link),
    {error, undefined} = so_digraph:get(Gb, <<"name/first">>),
    {ok, a2} = so_digraph:get(Gb, <<"firstname">>),
    {ok, _, Ga} = so_digraph:new(Gb, home, <<"/a">>, []),
    {ok, _} = so_digraph:new(Gb, <<"/a/b_name">>, <<"name">>, link),
    {ok, _} = so_digraph:new(Gb, <<"to_a">>, <<"/a">>, link),
    {ok, "/a/b"} = so_digraph:get(Ga, <<"b_name">>),
    {ok, 3} = so_digraph:get(Ga, count, []),
    {ok, a1} = so_digraph:get(G, <<"/a/b/to_a">>),
    ok = so_digraph:delete(Ga, in, link),
    {error, undefined} = so_digraph:get(Gb, <<"to_a">>),
    ok = so_digraph:delete(Ga, out, link),
    {error, undefined} = so_digraph:get(G, <<"/a/b">>),
    ok = so_digraph:delete(Ga, all, link),
    %% Ga is destruct
    ok = so_digraph:delete(Ga, []),
    {error, undefined} = so_digraph:delete(Ga, all, link),
    %% delete object
    ok = so_digraph:delete(Gb, <<"name">>),
    {error, undefined} = so_digraph:get(Gb, <<"name">>),
    %% exception
    {error, undefined} = so_digraph:delete(G, <<"/a/b/c/d">>),
    %% fatal link
    {ok, IdTest} = so_digraph:new(Gb, <<"test">>, test),  % normal link
    {ok, IdFirstname} = so_digraph:get(Gb, <<"firstname">>, id),  % fatal link
    ok = so_digraph:delete(Gb, []),
    {ok, test} = so_digraph:get(G, [IdTest]),
    {error, undefined} = so_digraph:get(G, [IdFirstname]),
    %% unlink fatal link before deletion
    {ok, _} = so_digraph:new(G, <<"y">>, [X], fatal_link),
    ok = so_digraph:delete(G, [Y2Z], link),
    ok = so_digraph:delete(G, <<"y">>),
    {error, undefined} = so_digraph:get(G, <<"/y/z">>),
    {ok, z} = so_digraph:get(G, <<"z">>),
    {error, undefined} = so_digraph:get(G, [X]).

helper_test() ->
    Path = [<<"a">>, <<"b">>, <<"c">>],
    <<"a/b/c/">> = so_digraph:encode(Path),
    <<"/a/b/c/">> = so_digraph:encode(Path, absolute),
    <<"a/b/c/">> = so_digraph:encode(Path, relative),
    Path = so_digraph:decode(<<"a/b/c">>),
    [<<>> | Path] = so_digraph:decode(<<"/a/b/c">>),
    Home = <<"home">>,
    Path = so_digraph:decode(<<"/a/b/c">>, Home),
    [Home | Path] = so_digraph:decode(<<"a/b/c">>, Home),
    Path = so_digraph:decode(<<"a/b/c">>, undefined).
    
    %% Name = <<"name">>,
    %% Name = so_digraph:short_name({from, Name}),
    %% Name = so_digraph:short_name(Name).

fatal_link_test() ->
    G1 = so_digraph:create([{default_link, [fatal]}]),
    G2 = so_digraph:create([{default_link, []}]),
    so_digraph:new(G1, <<"a">>, a),
    so_digraph:new(G1, <<"a/b">>, b),
    so_digraph:new(G1, <<"a/b/c">>, c),
    so_digraph:new(G1, <<"a/b/d">>, d),
    so_digraph:new(G1, <<"a/b/c/e">>, e),
    so_digraph:new(G2, <<"a">>, a),
    so_digraph:new(G2, <<"a/b">>, b),
    so_digraph:new(G2, <<"a/b/c">>, c),
    so_digraph:new(G2, <<"a/b/d">>, d),
    so_digraph:new(G2, <<"a/b/c/e">>, e),

    ?assert({ok, 5} =:= so_digraph:get(G1, count)),
    ?assert({ok, 4} =:= so_digraph:get(G1, count, link)),
    ?assert({ok, 5} =:= so_digraph:get(G2, count)),
    ?assert({ok, 4} =:= so_digraph:get(G2, count, link)),

    {ok, E1} = so_digraph:get(G1, <<"a/b/c/e">>, id),
    {ok, E2} = so_digraph:get(G2, <<"a/b/c/e">>, id),
    so_digraph:delete(G1, <<"a/b/c">>),
    ?assert({error, undefined} =:= so_digraph:get(G1, [E1])),
    so_digraph:delete(G2, <<"a/b/c">>),
    ?assert({ok, e} =:= so_digraph:get(G2, [E2])),
    
    {ok, B1} = so_digraph:get(G1, <<"a/b">>, id),
    {ok, B2} = so_digraph:get(G2, <<"a/b">>, id),
    so_digraph:delete(G1, <<"a">>),
    ?assert({error, undefined} =:= so_digraph:get(G1, [B1])),
    so_digraph:delete(G2, <<"a">>),
    ?assert({ok, b} =:= so_digraph:get(G2, [B2])),

    ?assert({ok, 0} =:= so_digraph:get(G1, count)),
    ?assert({ok, 0} =:= so_digraph:get(G1, count, link)),
    ?assert({ok, 3} =:= so_digraph:get(G2, count)),
    ?assert({ok, 1} =:= so_digraph:get(G2, count, link)).
    
threading_test() ->
    G = so_digraph:create([{module, sdigraph}, {home, [root]}]),
    Pid = self(),
    Test = fun() ->
                   ?assert({ok, 0} == so_digraph:get(G, count)),
                   ?assert({ok, root} == so_digraph:put(G, [], test, [])),
                   ?assert({ok, test} == so_digraph:get(G, [])),
                   ok = so_digraph:delete(G, []),
                   Pid ! test_end
           end,
    spawn(Test),
    receive
        test_end ->
            ok
    end.
