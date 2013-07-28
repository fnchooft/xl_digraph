%%%-------------------------------------------------------------------
%%% @author Gary Hai <gary@XL59.com>
%%% @copyright (C) 2013, XL59 Platforms, Inc.
%%% @doc
%%%  state object for digraph.
%%% Exported functions:
%%% @end
%%% Created : 17 Jun 2013 by Gary Hai <gary@XL59.com>
%%%-------------------------------------------------------------------
-module(so_digraph).

%% xl_state callbacks
-export([create/1, get/3, put/4, new/4, delete/3]).

%%% helper API
%-export([get/2, put/3, new/3, new/2, delete/2]).
-export([encode/1, encode/2, decode/1, decode/2]).

%%% exported types
-export_type([special/0, option/0, core/0, key/0]).

-include_lib("sop.hrl").

%%% for quick debug
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% definitions
-type option() :: 'object'     |  % entity
                  'id'         |  % force return vertex id only
                  'link'       |  % operation on edge
                  'fatal_link'.   % link with fatal flag
                  
-type opts() :: [option()].
-type special() :: 'all' | 'in' | 'out' |
                   'count' | 'in_degree' | 'out_degree' |
                   {'path', From::term(), To::term()} |
                   {'short_path', From::term(), To::term()} |
                   {'home', Home::term()}.
-type key() :: special() | list() | binary() | 'undefined'.

-record(core, {module = digraph :: module(),
               graph :: term(),
%               vital_link = true :: boolean(),
               home :: term()}).

-opaque core() :: #core{}.

-define(separator, <<"/">>).
-define(root, <<>>).
%-define(base, '$_base').
-define(fatal, fatal).
-define(DFL_LINK, []).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc data object constructor.
%%--------------------------------------------------------------------
-spec create(Args) -> core() when
      Args :: [{'core', core()}     |
               {'module', module()} |
               {'config', list()}   |
               {'graph', term()}    |
               {'vital_link', boolean()} |  % do not optimize early
               {'home', 'undefined' | binary() | list()}].

create(Args) ->
    Core = proplists:get_value(core, Args, #core{module = digraph}),
    Mod = proplists:get_value(module, Args, Core#core.module),
    Graph = case proplists:get_value(graph, Args, Core#core.graph) of
                undefined ->
                    Opts = proplists:get_value(config, Args, []),
                    Mod:new(Opts);
                G ->
                    G
            end,
    case proplists:get_value(home, Args) of
        undefined ->
            Home = Core#core.home;
        Loc ->
            {ok, Home} = take_home(Mod, Graph, undefined, Loc, true)
    end,
%    Vital = proplists:get_value(vital_link, Args, true),
    Core#core{module = Mod, graph = Graph, home = Home}.

%% do not create vertex more than 1 level
take_home(Mod, Graph, Home, Path, Create) ->
    case last_of(Mod, Graph, Home, Path) of
        {_E, _F, T, _V} ->
            {ok, T};
        {T} when Create ->
            case Mod:vertex(Graph, T) of
                false ->
                    T = Mod:add_vertex(Graph, T);
                _ ->
                    ok
            end,
            {ok, T};
        {T} ->
            {ok, T};
        _NotExisted ->
            {error, undefined}
    end.

%%--------------------------------------------------------------------
%% @doc Fetch data from graph.
%%      For global access, Key is ID of vertex or edge.
%%      For local access, Key is short name of edge.
%%--------------------------------------------------------------------
%%---- FSM or state object access.
-spec get(core(), key(), opts()) -> output().
%% parse arguments
get(#core{module = Mod, graph = Graph, home = Home}, Key, Options) ->
    iget(Mod, Graph, Home, Key, parse_option(Options)).

%% absolute addressing.
iget(Mod, G, undefined, Key, Option) ->
    g_get(Mod, G, Key, Option);
%% relative addressing
iget(Mod, G, Loc, Key, Option) ->
    l_get(Mod, G, Loc, Key, Option). % local get

%% atom as option, can be parsed by proplists too.
parse_option([]) ->  % default option is value
    object;
parse_option([id | _]) ->
    id;
parse_option([object | _]) ->
    object;
parse_option([value | _]) ->
    object;
parse_option([link | _]) ->
    link;
parse_option([fatal_link | _]) ->
    fatal_link;
parse_option([_ | Last]) ->
    parse_option(Last);
parse_option(NotList) ->
    parse_option([NotList]).


%%---- global graph operations.
%%--- special keys.
%% @doc get all edge return edge id list, not value list.
%%      get all vertices return list of {VertexId, Value}.
%%-- neighbores
g_get(Mod, G, all, link) ->
    {ok, Mod:edges(G)};
g_get(Mod, G, all, id) ->  % enumarate
    {ok, Mod:vertices(G)};
g_get(Mod, G, all, object) ->
    {ok, [Mod:vertex(G, V) || V <- Mod:vertices(G)]};
%% warning: when graph is deleted, no exception raised
g_get(Mod, G, count, link) ->
    case Mod:no_edges(G) of
        undefined ->
            {error, undefined};
        Count ->
            {ok, Count}
    end;
g_get(Mod, G, count, object) ->
    case Mod:no_vertices(G) of
        undefined ->
            {error, undefined};
        Count ->
            {ok, Count}
    end;
%%-- path
%%g_get(Mod, G, {path, From, To}, link) ->
%%-- navigate and fetch
g_get(Mod, G, Path, Option) ->
    l_get(Mod, G, undefined, Path, Option).

%%---- local graph operations.
%%--- special keys.
%%-- enumerate neighbours
l_get(Mod, G, Loc, all, link) ->
    {ok, Mod:edges(G, Loc)};
l_get(Mod, G, Loc, all, fatal_link) ->
    Fl = lists:filter(fun(E) ->
                              {E, _F, _T, V} = Mod:edge(G, E),
                              proplists:get_value(?fatal, V, false)
                      end, Mod:out_edges(G, Loc)),
    {ok, Fl};
l_get(Mod, G, Loc, all, id) ->
    In = Mod:in_neighbours(G, Loc),
    Out = Mod:out_neighbours(G, Loc),
    {ok, lists:append(In, Out)};
l_get(Mod, G, Loc, in, id) ->
    {ok, Mod:in_neighbours(G, Loc)};
l_get(Mod, G, Loc, in, link) ->
    {ok, Mod:in_edges(G, Loc)};
l_get(Mod, G, Loc, out, id) ->
    {ok, Mod:out_neighbours(G, Loc)};
l_get(Mod, G, Loc, out, link) ->
    {ok, Mod:out_edges(G, Loc)};
l_get(Mod, G, Loc, count, _) ->
    {ok, Mod:in_degree(G, Loc) + Mod:out_degree(G, Loc)};
l_get(Mod, G, Loc, in_degree, _) ->
    {ok, Mod:in_degree(G, Loc)};
l_get(Mod, G, Loc, out_degree, _) ->
    {ok, Mod:out_degree(G, Loc)};
%%--- local lookup, by cascading names.
l_get(Mod, G, Loc, Path, Option) ->
    case last_of(Mod, G, Loc, Path) of
        {_E, _F, T, _V} when Option == id ->
            {ok, T};
        {_E, _F, _T, _V} = Edge when Option == link ->
            {ok, Edge};
        {_E, _F, T, _V} ->
            {T, V} = Mod:vertex(G, T),
            {ok, V};
        %%-- when Path is []
        {Edge} when Option == link ->  % only valid when Loc is undefined
            case Mod:edge(G, Edge) of
                false ->
                    {error, undefined};
                LinkInfo ->
                    {ok, LinkInfo}
            end;
        {Home} when Option == id ->  % get home id
            {ok, Home};
        {Home} ->
            case Mod:vertex(G, Home) of
                {_, V} ->
                    {ok, V};
                false ->
                    {error, undefined}
            end;
        _ ->
            {error, undefined}
    end.

%%--- iterate get last edge (or vertex) by local name path.
%% binary type, decode to list type.
%% <<"/a/b/c">> ==> [<<>>, <<"a">>, <<"b">>, <<"c">>], absolute addressing.
%% <<"a/b/c">> ==> [<<"a">>, <<"b">>, <<"c">>], relative addressing.
last_of(Mod, Graph, Base, Path) when is_binary(Path) ->
    Key = decode(Path),
    last_of(Mod, Graph, Base, Key);
%% absolute addressing
last_of(Mod, Graph, _Base, [?root | Path]) ->
    last_of(Mod, Graph, undefined, Path);
%% [] is invalid when Base is undefined
last_of(_M, _G, undefined, []) ->
    false;
last_of(Mod, Graph, undefined, [Base | Path]) ->
    last_of(Mod, Graph, Base, Path);
last_of(_M, _G, Base, []) ->
    {Base};  % get home location
last_of(Mod, Graph, Base, [Last | []]) ->
    case Mod:edge(Graph, {Base, Last}) of
        false ->
            {Base, Last};  % the last edge is not existed.
        Full ->
            Full
    end;
last_of(Mod, Graph, Base, [ShortName | Rest]) ->
    Edge = {Base, ShortName},
    case Mod:edge(Graph, Edge) of
        {Edge, Base, To, _Value} ->
            last_of(Mod, Graph, To, Rest);
        false ->
            false
    end;
last_of(_M, _G, _B, _Unknown) ->
    false.


%%--------------------------------------------------------------------
%% @doc Update data in graph.
%%--------------------------------------------------------------------
%%---- put API
-spec put(core(), key(), term(), opts()) -> output().
%% new relative location.
%% notice: put function to change home does not check existence of new location.
put(#core{module = M, graph = G, home = Home} = Core, home, Loc, _Opt) ->
    case take_home(M, G, Home, Loc, false) of
        {ok, NewHome} ->
            {ok, updated, Core#core{home = NewHome}};
        Error ->
            Error
    end;
%% Parse arguments
put(#core{module = M, graph = G, home = H}, Key, Value, Options) ->
    iput(M, G, H, Key, Value, parse_option(Options)).

%% update edge value
%% notice: put(..., link) update link value,
%% put(..., true/false, fatal_link) change fatal flag only
iput(Mod, G, Loc, Path, Value, link) ->
    case last_of(Mod, G, Loc, Path) of
        {E, F, T, _} ->
            E = Mod:add_edge(G, E, F, T, Value),
            {ok, E};
        {Edge} ->
            case Mod:edge(G, Edge) of
                {Edge, From, To, _} ->
                    {ok, Mod:add_edge(G, Edge, From, To, Value)};
                false ->
                    {error, undefined}
            end;
        _ ->
            {error, undefined}
    end;
iput(Mod, G, Loc, Path, Value, fatal_link) ->  %% Value is true|false
    case last_of(Mod, G, Loc, Path) of
        {E, F, T, Flags} ->
            Fl = proplists:delete(?fatal, Flags),
            NewFlags = if Value -> [?fatal | Fl]; true -> Fl end,
            E = Mod:add_edge(G, E, F, T, NewFlags),
            {ok, E};
        _ ->
            {error, undefined}
    end;
iput(Mod, G, Loc, Path, Value, object) ->
    case last_of(Mod, G, Loc, Path) of
        {_E, _F, T, _V} ->
            T = Mod:add_vertex(G, T, Value),
            {ok, T};
        {Obj} ->
            Obj = Mod:add_vertex(G, Obj, Value),
            {ok, Obj};
        _ ->
            {error, undefined}
    end;
iput(_, _, _, _, _, _) ->
    {error, badarg}.

%%--------------------------------------------------------------------
%% @doc Create items in graph.
%%--------------------------------------------------------------------
%%---- new API
-spec new(core(), key(), term(), opts()) -> output().
%% new state object with new home location.
new(Core, home, Loc, Opt) ->
    put(Core, home, Loc, Opt);
%% parse arguments
new(#core{module = M, graph = G, home = H}, Key, Value, Options) ->
    inew(M, G, H, Key, Value, parse_option(Options)).

%%---- internal functions for new API
%%--- global creating operation
%%-- create link, value of new link: {From, To}
%% need generate id of link
inew(M, G, H, undefined, Value, Opt) ->
    inew(M, G, H, [], Value, Opt);
inew(M, G, H, Key, Value, Link) when Link == link; Link == fatal_link ->
    Flags = if Link == fatal_link -> [?fatal]; true -> [] end,
    new_link(M, G, H, Key, Value, Flags);
%%-- generate id and create new vertex
%%-- create new vertex
inew(Mod, G, undefined, Path, Value, object) ->
    case last_of(Mod, G, undefined, Path) of
        {Base} ->  % warning: no check of existence here
            {ok, Mod:add_vertex(G, Base, Value)};
        {Base, Key} ->
            inew(Mod, G, Base, [Key], Value, object);
        {_E, _F, _T, _V} ->  % object is existence
            {error, already_exists};
        false when Path == <<>>; Path == []; Path == undefined ->
            NewVertex = Mod:add_vertex(G),  % generate id
            {ok, Mod:add_vertex(G, NewVertex, Value)};
        false ->
            {error, badarg}
    end;
%%-- create new vertex and link to current vertex
inew(Mod, G, Loc, Path, Value, object) ->
    NewV = Mod:add_vertex(G),
    NewV = Mod:add_vertex(G, NewV, Value),
    case new_link_by_id(Mod, G, Loc, Path, NewV, ?DFL_LINK) of
        {error, _} = Err ->
            Mod:del_vertex(G, NewV),  % cleanup
            Err;
        _Ok ->
            {ok, NewV}
    end;
inew(_, _, _, _, _, _) ->
    {error, badarg}.

%% warning: if key is existed, return error
new_link(M, G, H, Path, To, Flags) ->
    case iget(M, G, H, To, id) of
        {ok, Ti} ->
            new_link_by_id(M, G, H, Path, Ti, Flags);
        Error ->
            Error
    end.

new_link_by_id(M, G, H, Path, Ti, Flags) ->
    case last_of(M, G, H, Path) of
        {NewL, _} = Key ->  % last key is not existed
            case M:add_edge(G, Key, NewL, Ti, Flags) of
                {error, _} = Err ->
                    Err;
                Key ->
                    {ok, Key}
            end;
        {Fi} ->
            case M:add_edge(G, Fi, Ti, Flags) of
                {error, _} = Err ->
                    Err;
                Key ->
                    {ok, Key}
            end;
        {_, _, _, _} ->  % last key is existed.
            {error, already_exists};
        false ->  % path is broken
            {error, unreachable}
    end.

%%--------------------------------------------------------------------
%% @doc Delete item in graph.
%%--------------------------------------------------------------------
%%---- delete API
-spec delete(core(), key(), opts()) -> output().
delete(#core{module = M, graph = G, home = H}, Key, Options) ->
    idelete(M, G, H, Key, parse_option(Options)).

%%---- global graph operations.
%%--- special keys.
%% delete the graph, clean all data.
idelete(Mod, G, undefined, all, object) ->
    Mod:delete(G), ok;
%%---- local graph operations.
idelete(Mod, G, Loc, all, link) ->
    %% remove a vertex cause all edges deleted.
    case Mod:vertex(G, Loc) of
        {Loc, Value} ->
            true = Mod:del_vertex(G, Loc),
            Loc = Mod:add_vertex(G, Loc, Value), ok;
        false ->
            {error, undefined}
    end;
idelete(Mod, G, Loc, in, link) ->
    Mod:del_edges(G, Mod:in_edges(G, Loc)), ok;
idelete(Mod, G, Loc, out, link) ->
    Mod:del_edges(G, Mod:out_edges(G, Loc)), ok;
%%--- local lookup, by cascading names.
idelete(Mod, G, Loc, Path, Option) ->
    case last_of(Mod, G, Loc, Path) of
        {Edge, _F, _T, _V} when Option == link ->
            Mod:del_edge(G, Edge), ok;
        {_E, _F, T, _V} ->
            r_del(Mod, G, T), ok;
        %% explicitly remove link by full name of the link
        {Edge} when Option == link ->  
            Mod:del_edge(G, Edge), ok;
        {Vertex} when Option == object ->  % self-destructure
            r_del(Mod, G, Vertex),ok;
        _ ->  % unknown or broken path
            {error, undefined}
    end.

%% warning: not tail recursion
r_del(Mod, G, Id) ->
    {ok, Fl} = l_get(Mod, G, Id, all, fatal_link),
    Fil = lists:map(fun(E) ->
                            {_, _F, T, _} = Mod:edge(G, E),
                            T
                    end, Fl),
    Mod:del_vertex(G, Id),
    [r_del(Mod, G, Fi) || Fi <- Fil].

%%%===================================================================
%%% Helper APIs
%%%===================================================================
%% get(Core, Key) ->
%%     get(Core, Key, []).

%% put(Core, Key, Value) ->
%%     put(Core, Key, Value, []).

%% new(Core, Key, Value) ->
%%     new(Core, Key, Value, []).

%% new(Core, Value) ->
%%     new(Core, undefined, Value, []).

%% delete(Core, Key) ->
%%     delete(Core, Key, []).

%%--------------------------------------------------------------------
%% @doc Parse binary string between cascading links
%%--------------------------------------------------------------------
-spec encode(list()) -> binary().
encode(Path) ->
    lists:foldr(fun(E, Acc) ->
                        <<E/binary,
                          ?separator/binary,
                          Acc/binary>> end, <<>>, Path).

-spec encode(list(), 'relative' | 'absolute') -> binary().
encode(Path, absolute)->
    RPath = encode(Path),
    <<?separator/binary, RPath/binary>>;
encode(Path, _) ->
    encode(Path).                        

-spec decode(binary()) -> list().
decode(Path) when is_binary(Path) ->
    binary:split(Path, ?separator, [global, trim]).

-spec decode(binary(), term()) -> list().
%% absolute path if first char is "/", otherwise complete the relative path.
decode(Path, Home) ->
    case decode(Path) of
        [<<>> | Rest] ->
            Rest;
        NoHome when Home == undefined ->
            NoHome;
        RelativePath ->
            [Home | RelativePath]
    end.

