{application, drink,
 [{description, "Drink Server"},
  {vsn, "1.0"},
  {modules, [drink_app, drink_sup, drink_machines_sup, drink_machine,
			 drink_machine_comm, drink_web, gen_listener, sunday_server, user_auth]},
  {registered,[drink_sup, drink_machines_sup, drink_machine_comm_listener, user_auth, sunday_server_listener]},
  {applications, [kernel, stdlib, sasl, ssl]},
  {mod, {drink_app, []}},
  {start_phases, []}
]}.
