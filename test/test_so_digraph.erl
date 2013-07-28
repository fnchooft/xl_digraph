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

get(Graph, Key) ->
    get(Graph, Key, []).
get(Graph, Key, Options) ->
    so_digraph:get(Graph, Key, Options).
put(Graph, Key, Value, Options) ->
    so_digraph:put(Graph, Key, Value, Options).
new(Graph, Key, Value, Options) ->
    so_digraph:new(Graph, Key, Value, Options).
del(Graph, Key, Options) ->
    so_digraph:delete(Graph, Key, Options).

init_test() ->
    G1= so_digraph:create([]),
    ?assert({ok, 0} == get(G1, count)),
    ?assert(ok == del(G1, all, object)),
%    ?assertError({ok, 0} == get(G1, count)),
    ?assert({error, undefined} == get(G1, count)),

    G0 = digraph:new(),
    digraph:add_vertex(G0, test, true),
    G2 = so_digraph:create([{graph, G0}]),
    ?assert({ok, 1} == get(G2, count)),
    ?assert({ok, test} == get(G2, [test], id)),
    ?assert({ok, true} == get(G2, [test])),
    ?assert(ok == del(G2, all, object)),
    
    G3= so_digraph:create([{home, [root]}]),
    ?assert({ok, 0} == get(G3, count)),
    ?assert({ok, root} == put(G3, [], test, [])),
    ?assert({ok, test} == get(G3, [])),
    ok = del(G3, [], object).
    
main_test() ->
    G = so_digraph:create([]),
    %%---- new test
    %%--- global access, without home
    {ok, X} = new(G, <<>>, x, object),  % generate id
    {ok, Y} = new(G, <<"y">>, y, object),
    {ok, Z} = new(G, <<"z">>, z, object),
    {ok, _} = new(G, <<"a">>, a, object),
    {ok, _} = new(G, <<"b">>, b, object),
    {ok, AB} = new(G, <<"a/b">>, ab, object),  % relative
    {ok, ABC} = new(G, <<"/a/b/c">>, abc, object),  % absolute
    %% link X to Y with name x_y as fatal link
    {ok, _} = new(G, [X, <<"x_y">>], Y, fatal_link),
    {ok, _} = new(G, Y, Z, link),  % link Y to Z with no edge label special
    {ok, _} = new(G, <<"a/c">>, <<"a/b/c">>, link), % link a to a/b/c by name c.
    {error, already_exists} = new(G, <<"a/c">>, <<"a/b/c">>, link),
    {error, already_exists} = new(G, [X, <<"x_y">>], Y, link),
    {ok, Y2Z} = new(G, Y, Z, fatal_link),  % can link Y to Z with different name
    {error, already_exists} = new(G, <<"/a/b">>, ab, object),

    %%--- local access, with home
    {ok, _, Gb} = new(G, home, <<"/a/b">>, id),
    {ok, ab} = get(Gb, <<>>),
    {ok, b} = get(G, <<"b">>),
    {error,unreachable} = new(Gb, <<"a/y/x">>, nopath, object),
    {error, already_exists} = new(Gb, <<"c">>, [<<>>, ABC], link),
    {error, already_exists} = new(Gb, <<"c">>, <<"/a/b/c">>, link),
    {ok, Name} = new(Gb, <<"name">>, "/a/b", object),
    {ok, Firstname} = new(Gb, <<"name/first">>, "a", object),
    {ok, _} = new(Gb, <<"firstname">>, <<"name/first">>, fatal_link),
    {ok, "a"} = get(Gb, <<"firstname">>),

    %%---- get test
    %%--- special key: global
    {ok, 9} = get(G, count),
    {ok, 9} = get(G, count, link),
    {ok, AllLink} = get(G, all, link),
    {ok, AllId} = get(G, all, id),
    {ok, AllObj} = get(G, all),
    FnLink = {{AB, <<"firstname">>}, AB, Firstname, [fatal]},
    {ok, FnLink} = get(G, <<"/a/b/firstname">>, link),
    {ok, FnLink} = get(G, [{AB, <<"firstname">>}], link),
    {error, undefined} = get(G, <<"not_exists">>, link),

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
    {ok, AB} = get(Gb, <<>>, id),
    {ok, FnLink} = get(Gb, <<"firstname">>, link),
    {error, undefined} = get(Gb, <<"nothere">>),
    {error, undefined} = get(Gb, <<"/nothere">>),
    {ok, abc} = get(Gb, <<"/a/b/c">>),  % absolute addressing
    {ok, abc} = get(Gb, <<"c">>),  % relative addressing
    
    {ok, 1} = get(Gb, in_degree),
    {ok, 3} = get(Gb, out_degree),
    {ok, 4} = get(Gb, count),
    {ok, Links} = get(Gb, all, link),
    {ok, FatalLinks} = get(Gb, all, fatal_link),
    {ok, Ids} = get(Gb, all, id),
    {ok, InIds} = get(Gb, in, id),
    {ok, InLinks} = get(Gb, in, link),
    {ok, OutIds} = get(Gb, out, id),
    {ok, OutLinks} = get(Gb, out, link),

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
    {ok, _} = put(G, [{AB, <<"name">>}], [fatal], link),
    {ok, Fl} = get(Gb, all, fatal_link),
    true = lists:member({AB, <<"name">>}, Fl),
    {error, undefined} = put(G, <<"unknown">>, nothing, link),
    {error, undefined} = put(Gb, <<"unknown">>, nothing, link),
    {ok, _} = put(Gb, <<"name">>, [], link),
    {ok, Fl1} = get(Gb, all, fatal_link),
    false = lists:member({AB, <<"name">>}, Fl1),
    %% update fatal flag only
    {ok, _} = put(Gb, <<"name">>, true, fatal_link),
    {ok, Fl} = get(Gb, all, fatal_link),  % already tested
    {error, undefined} = put(Gb, <<"unknown">>, true, fatal_link),
    %% --- value update
    {ok, _} = put(G, <<"a">>, a1, object),
    {ok, a1} = get(G, <<"a">>),
    {ok, "a"} = get(Gb, <<"firstname">>),
    {ok, _} = put(G, <<"/a/b/firstname">>, a2, object),
    {ok, a2} = get(G, <<"/a/b/name/first">>),
    {ok, _} = put(Gb, <<>>, "AB", object),  % change the home value
    {ok, "AB"} = get(Gb, []),
    {error, undefined} = put(Gb, <<"unknown">>, nothing, object),
    {ok, _, Gname} = put(Gb, home, <<"name">>, []),
    {ok, a2} = get(Gname, <<"first">>),
    {ok, _, Gnothere} = put(G, home, <<"nothere">>, []),
    %% nothere is not existed in the graph
    {error, undefined} = get(Gnothere, []),
    {error, undefined} = put(Gnothere, <<"hello">>, world, object),
    %% put or new operation may create it.
    {ok, _} = put(Gnothere, [], anything, object),
    {ok, anything} = get(Gnothere, []),
    {ok, _} = new(Gnothere, <<"hello">>, world, object),
    {ok, world} = get(Gnothere, <<"hello">>), 
    %% cascading location is forbidden for new home
    {error, undefined} = put(G, home, <<"not/here">>, []),
    
    %%---- delete test
    %%--- link deletion
    %% delete link directly by link id.
    {ok, abc} = get(G, <<"/a/c">>),
    ok = del(G, <<"a/c">>, link),    
    {error, undefined} = get(G, <<"/a/c">>),
    {ok, a2} = get(Gb, <<"name/first">>),
    ok = del(Gb, <<"name/first">>, link),
    {error, undefined} = get(Gb, <<"name/first">>),
    {ok, a2} = get(Gb, <<"firstname">>),
    {ok, _, Ga} = new(Gb, home, <<"/a">>, []),
    {ok, _} = new(Gb, <<"/a/b_name">>, <<"name">>, link),
    {ok, _} = new(Gb, <<"to_a">>, <<"/a">>, link),
    {ok, "/a/b"} = get(Ga, <<"b_name">>),
    {ok, 3} = get(Ga, count, []),
    {ok, a1} = get(G, <<"/a/b/to_a">>),
    ok = del(Ga, in, link),
    {error, undefined} = get(Gb, <<"to_a">>),
    ok = del(Ga, out, link),
    {error, undefined} = get(G, <<"/a/b">>),
    ok = del(Ga, all, link),
    %% Ga is destruct
    ok = del(Ga, [], object),
    {error, undefined} = del(Ga, all, link),
    %% delete object
    ok = del(Gb, <<"name">>, object),
    {error, undefined} = get(Gb, <<"name">>),
    %% exception
    {error, undefined} = del(G, <<"/a/b/c/d">>, object),
    %% fatal link
    {ok, IdTest} = new(Gb, <<"test">>, test, object),  % normal link
    {ok, IdFirstname} = get(Gb, <<"firstname">>, id),  % fatal link
    ok = del(Gb, [], object),
    {ok, test} = get(G, [IdTest]),
    {error, undefined} = get(G, [IdFirstname]),
    %% unlink fatal link before deletion
    {ok, _} = new(G, <<"y">>, [X], fatal_link),
    ok = del(G, [Y2Z], link),
    ok = del(G, <<"y">>, object),
    {error, undefined} = get(G, <<"/y/z">>),
    {ok, z} = get(G, <<"z">>),
    {error, undefined} = get(G, [X]).

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

%% threading_test() ->
%%     {ok, G}= so_digraph:init([{module, xl_digraph}, {home, [root]}]),
%%     Pid = self(),
%%     Test = fun() ->
%%                    ?assert({ok, 0} == get(G, count)),
%%                    ?assert({ok, root} == put(G, [], test, [])),
%%                    ?assert({ok, test} == get(G, [])),
%%                    ok = del(G, [], object),
%%                    Pid ! test_end
%%            end,
%%     spawn(Test),
%%     receive
%%         test_end ->
%%             ok
%%     end.
