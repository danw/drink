%%%-------------------------------------------------------------------
%%% File    : drink_mnesia.hrl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : 
%%%
%%%
%%% edrink, Copyright (C) 2008 Dan Willemsen
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-record (machine, {machine, password, name, public_ip, available_sensor = false, machine_ip, allow_connect = true, admin_only = false}).
-record (slot, {machine, num, name, price = 10000, avail = 0, disabled = false}).

-record (temperature, {machine, time, temperature}).
-record (money_log, {time, username, admin = nil, amount, direction = out, reason}).
-record (drop_log, {machine, slot, username, time, status = ok}).