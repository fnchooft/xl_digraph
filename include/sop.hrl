%%%-------------------------------------------------------------------
%%% @author Gary Hai <gary@XL59.com>
%%% @copyright (C) 2011, XL59 Platforms, Inc.
%%% @doc
%%% Definitions:
%%%  %debug debug switch.
%%% @end
%%% Created : 24 Apr 2012 by Gary Hai <gary@XL59.com>
%%%-------------------------------------------------------------------

%%% debug switch
%% -ifndef(debug).
%% -define(debug, true).
%% -endif.

%%% predefined state method function to wrap state object.
%%% it is the anchor of sop framework.
%%% compile warning will be raised when STATE is re-defined.
%-define(STATE, fun state/2).

%%% macros for reserved words
-define(break, break).
-define(exception, exception).
-define(call, '$so_call').
-define(reply, '$so_reply').


%%% system type
-type fsm()       :: atom() | pid() | {atom(), atom()}.

%%% state behaviour related types
-type directive() :: 'ok' | 'error' | 'stop' | 'pending' | term().
-type output()    :: 'ok' | {directive(), Output::term()} |
                     {directive(), Output::term(), NewState::term()}.
-type state()     :: fun((Input::term()) -> output()).
-type object()    :: state().
-type action()    :: fun((state(), Input::term()) -> output()).

%% option may be one atom value.
-type options()   :: atom() | proplists:proplist().
