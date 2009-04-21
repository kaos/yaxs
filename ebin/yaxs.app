%% -*- erlang -*-
{ application, yaxs,
  [{ description, "Yet Another XMPP Server" },
   { vsn, "0.1" },
   { modules, [ yaxs, yaxs_app, yaxs_sup, yaxs_con, yaxs_client ]},
   { registered, [ yaxs_sup, yaxs_con, yaxs_client_sup ]},
   { applications, [ kernel, stdlib, sasl ]},
   { mod, { yaxs_app, []}},
   { start_phases, []}
  ]
 }.
