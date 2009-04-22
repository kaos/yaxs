%%%-------------------------------------------------------------------
%%% File    : yaxs_sax.erl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%%
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>
%%%-------------------------------------------------------------------
-module(yaxs_sax).

%% API
-export([parse/2, parse/3]).

-record(sax_state, {
	  parser
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
parse(Xml, Cb) ->
    Pid = self(),
    #sax_state{
       parser = erlsom:parse_sax(Xml, 
				 fun (Event) -> Cb(Pid, Event) end,
				 fun sax_event/2,
				 [
				  {continuation_function,
				   fun ([], S) -> {suspend, S};
				       (T, S) -> {T, S} % doesn't support incomplete fragments yet...
				   end,
				   []}
				 ])
      }.

parse(Xml, Cb, undefined) ->
    parse(Xml, Cb);
parse(Xml, _, #sax_state{ parser={suspend, State, Resume} } = Sax) ->
    Sax#sax_state{
      parser = Resume(Xml, State)
     };
parse(_, _, Sax) ->
    throw({sax_error, Sax}).

%%====================================================================
%% Internal functions
%%====================================================================

%% stream tag
sax_event({startElement, "http://etherx.jabber.org/streams", 
	   "stream", "stream", Attrs}, Fun) ->
    Fun({open_stream, Attrs}),
    Fun;

sax_event(endDocument, Fun) ->
    Fun(close),
    Fun;

%% other
sax_event(_Event, Fun) ->
    Fun.
