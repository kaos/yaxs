%%%-------------------------------------------------------------------
%%% File    : yaxs_core_stream.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_core_stream).

-include("yaxs.hrl").

%% API
-behaviour(yaxs_mod).
-export([
	 init/0,
	 handle/2
]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

init() ->
    yaxs_mod:register(?MODULE, [
				"http://etherx.jabber.org/streams"
			       ]).

handle({"http://etherx.jabber.org/streams", "stream", "stream", Attrs},
       #yaxs_client{ response=R } = Client) ->
    error_logger:info_msg("core open_stream: ~p~n", [Attrs]),
    R("\
<?xml version='1.0'?>\
<stream:stream\
 from='example.com'\
 id='someid'\
 xmlns='jabber:client'\
 xmlns:stream='http://etherx.jabber.org/streams'\
 version='1.0'>

 <stream:features>"),

    Tags = yaxs_event:publish(stream_features, Client),

    R("</stream:features>"),
    {tag, Tags}.


%%====================================================================
%% Internal functions
%%====================================================================
