{application, drink,
 [{description, "Drink Server"},
  {vsn, "0.0"},
  {modules, [drink_app, drink_sup, drink_machines_sup, drink_machine, drink_machine_listener,
			 drink_machine_comm, drink_web, user_auth]},
  {registered,[drink_sup, drink_machines_sup, drink_machine_listener, user_auth, sunday_server_listener]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {drink_app, []}},
  {start_phases, []}
]}.