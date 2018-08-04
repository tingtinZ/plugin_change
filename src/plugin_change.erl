-module(plugin_change).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = plugin_change_prv:init(State),
    {ok, State1}.
