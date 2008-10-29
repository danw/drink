{application, drink,
 [{description, "Drink Server"},
  {vsn, "0.0"},
  {modules, [drink_app, drink_sup, drink_machines_sup, drink_machine, drink_machine_listener,
			 drink_machine_communicator, user_auth, users_sup, drink_user]},
  {registered,[drink_sup, drink_machines_sup, drink_machine_listener, user_auth, users_sup]},
  {applications, [kernel, stdlib]},
  {mod, {drink_app, []}},
  {start_phases, []}
]}.