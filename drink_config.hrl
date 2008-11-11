-define(LISTEN_PORT, 4343).

-record (slot_info, {id, name, price = 10000, avail = false}).