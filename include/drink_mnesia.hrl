-record (machine, {machine, password, public_ip, available_sensor = false}).
-record (slot, {machine, num, name, price = 10000, avail = 0}).

-record (temperature, {machine, time, temperature}).
-record (money_log, {time, username, admin = nil, amount, direction = out, reason}).
-record (drop_log, {machine, slot, username, time, status = ok}).