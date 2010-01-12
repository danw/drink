%%%-------------------------------------------------------------------
%%% File    : user_auth_ldap.erl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : LDAP connector for user_auth.erl
%%%
%%%
%%% edrink, Copyright (C) 2008-2010 Dan Willemsen
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

-module (user_auth_ldap).

-export ([init/0]).
-export ([get_user/2]).
-export ([set_credits/1, set_admin/1, add_ibutton/2, del_ibutton/2]).

-include ("user.hrl").

init() -> ok.

% User Auth Interfaces
get_user(username, Username) ->
    get_ldap_user("uid", Username);
get_user(ibutton, Ibutton) ->
    get_ldap_user("ibutton", Ibutton).

set_credits(UserInfo) ->
    eldap:modify(eldap_user, 
        "uid=" ++ UserInfo#user.username ++ ",ou=users,dc=csh,dc=rit,dc=edu",
        [eldap:mod_replace("drinkBalance", [val_to_ldap_attr(credits, UserInfo#user.credits)])]).

set_admin(UserInfo) ->
    eldap:modify(eldap_user,
        "uid=" ++ UserInfo#user.username ++ ",ou=users,dc=csh,dc=rit,dc=edu",
        [eldap:mod_replace("drinkAdmin", [val_to_ldap_attr(admin, UserInfo#user.admin)])]).

add_ibutton(Username, IButton) ->
    eldap:modify(eldap_user,
        "uid=" ++ Username ++ ",ou=users,dc=csh,dc=rit,dc=edu",
        [eldap:mod_add("ibutton", [val_to_ldap_attr(ibutton, IButton)])]).

del_ibutton(Username, IButton) ->
    eldap:modify(eldap_user,
        "uid=" ++ Username ++ ",ou=users,dc=csh,dc=rit,dc=edu",
        [eldap:mod_delete("ibutton", [val_to_ldap_attr(ibutton, IButton)])]).

% Internal Methods

val_to_ldap_attr(credits, Val) ->
    integer_to_list(Val);
val_to_ldap_attr(admin, Val) ->
    case Val of
        true ->
            "1";
        false ->
            "0"
    end;
val_to_ldap_attr(ibutton, Val) ->
    Val.

ldap_attribute(Attr, {eldap_entry, _Dn, Attrs}) ->
	ldap_attribute(Attr, Attrs);
ldap_attribute(Attr, []) ->
	ldap_attribute_val(Attr, undefined);
ldap_attribute(Attr, [{Name, ValueArr}|T]) ->
	case string:equal(Attr, Name) of
		true ->
			ldap_attribute_val(Attr, ValueArr);
		false ->
			ldap_attribute(Attr, T)
	end.

ldap_attribute_val("ibutton", undefined) ->
    [];
ldap_attribute_val("ibutton", Val) ->
	Val;
ldap_attribute_val("drinkAdmin", undefined) ->
    false;
ldap_attribute_val("drinkAdmin", Val) ->
	case hd(hd(Val)) of
		$1 -> true;
		$0 -> false
	end;
ldap_attribute_val("drinkBalance", undefined) ->
    0;
ldap_attribute_val("drinkBalance", Val) ->
	{Int, _Extra} = string:to_integer(hd(Val)),
	Int;
ldap_attribute_val(_Attr, Val) ->
	hd(Val).

get_ldap_user(Attr, Value) when is_list(Attr), is_list(Value) ->
	Base = {base, "ou=users,dc=csh,dc=rit,dc=edu"},
	Scope = {scope, eldap:singleLevel()},
	Filter = {filter, eldap:equalityMatch(Attr, Value)},
	case eldap:search(eldap_user, [Base, Scope, Filter]) of
		{eldap_search_result, [ResultList], []} ->
		    Username = ldap_attribute("uid", ResultList),
			Admin = ldap_attribute("drinkAdmin", ResultList),
			Credits = ldap_attribute("drinkBalance", ResultList),
			IButtons = ldap_attribute("ibutton", ResultList),
			{ok, #user{username=Username,admin=Admin,credits=Credits,ibuttons=IButtons}};
		{eldap_search_result, [], []} ->
		    {error, invalid_user};
		{error, Reason} ->
			{error, Reason};
		Reason ->
			{error, Reason}
	end.

