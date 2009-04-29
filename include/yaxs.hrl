%%% File    : yaxs.hrl
%%% Author  : Andreas Stenius <kaos@astekk.se>
%%% Description : 
%%% Created : 22 Apr 2009 by Andreas Stenius <kaos@astekk.se>

-record(yaxs_client, 
	{
	  pid,
	  sock,
	  addr,
	  jid,
	  tags=[],
	  response
	 }).

-record(tag, {
	  namespace,
	  prefix,
	  name,
	  attrs=[],
	  body=[]
	 }).

-record(attribute, {
	  name,
	  prefix,
	  uri,
	  value
	 }).

-record(stanza, {
	  kind,
	  type,
	  to,
	  from,
	  id,
	  xml_lang,
	  body=[]
	 }).
