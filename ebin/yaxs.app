%% -*- erlang -*-
{ application, yaxs,
  [{ description, "Yet Another XMPP Server" },
   { vsn, "0.1" },
   { modules, [ yaxs ]},
   { registered, [ yaxs_sup ]},
   { applications, [ kernel, stdlib, sasl ]},
   { mod, { yaxs, []}},
   { start_phases, []}
  ]
 }.
