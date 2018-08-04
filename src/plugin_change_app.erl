%%%-------------------------------------------------------------------
%% @doc plugin_change public API
%% @end
%%%-------------------------------------------------------------------

-module(plugin_change_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    plugin_change_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
