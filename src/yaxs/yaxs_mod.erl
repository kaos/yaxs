%%% File    : yaxs_mod.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>

-module(yaxs_mod).

-export([
	 behaviour_info/1,
	 init/0,
	 register/2
	]).

behaviour_info(callbacks) ->
    [{init,0}, {handle,2}].

init() ->
    {ok, Mods} = application:get_env(yaxs, mods),
    [Mod:init() || Mod <- Mods].

register(Module, Events) ->
    yaxs_event:register(Module, Events).
