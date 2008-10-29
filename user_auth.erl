-module (user_auth).
-behaviour (gen_server).

-export ([start_link/0]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([user/1, auth/2, admin/2, user_info/1, delete_ref/1]).

-include ("ldapconf.hrl").
-include ("user.hrl").

-record (uastate, {ldapconn,reftable,usertable}).

%%%%%%%%%%%%%%%%%%%%%%%%
% Gen_Server Callbacks %
%%%%%%%%%%%%%%%%%%%%%%%%
start_link () ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init ([]) ->
	process_flag(trap_exit, true),
	RefTable = ets:new(ref2user, [set, private]),
	UserTable = ets:new(userinfo, [set, private, {keypos, 2}]),
	case eldap:open([?LDAPHOST], [	
%									{log, fun(_L, F, A) -> error_logger:info_msg(F, A) end},
%	 								{ssl, true}, Probably need to get self signed certs working...
%									{port, 636},
									{timeout, 5000}]) of
		{ok, LdapConn} ->
			case eldap:simple_bind(LdapConn, ?LDAPBINDDN, ?LDAPBINDPASS) of
				ok ->
					error_logger:info_msg("Logged into LDAP!!!!"),
					{ok, #uastate{ldapconn = LdapConn,
								  reftable = RefTable,
								  usertable = UserTable}};
				Reason ->
					{stop, Reason}
			end;
		{error, Reason} ->
			{stop, Reason};
		Reason ->
			{stop, Reason}
	end.

handle_call ({user, Username}, _From, State) when is_list(Username) ->
	case get_user(Username, State) of
		{ok, User} ->
			{reply, {ok, create_user_ref(User, read, State)}, State};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({auth, Username, Password}, _From, State) when is_list(Username), is_list(Password) ->
	case get_user(Username, State) of
		{ok, User} ->
			case string:equal(Password, "magicpass") of
				true ->
					{reply, {ok, create_user_ref(User, [read, drop, authed], State)}, State};
				false ->
					{reply, {error, badpass}, State}
			end;
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({admin, Admin, Username}, _From, State) when is_reference(Admin), is_list(Username) ->
	case get_user(Username, State) of
		{ok, User} ->
			case is_admin(Admin, State) of
				true ->
					{reply, {ok, create_user_ref(User, [read, write, ibutton], State)}, State};
				false ->
					{reply, {error, permission_denied}, State}
			end;
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({user_info, UserRef}, _From, State) when is_reference(UserRef) ->
	case get_from_ref(UserRef, State) of
		{ok, UserInfo, Perms} ->
			{reply, {ok, respect_read_permissions(UserInfo, #user{}, Perms)}, State};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call (_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

handle_cast ({delete_ref, UserRef}, State) when is_reference(UserRef) ->
	ets:delete(State#uastate.reftable, UserRef),
	{noreply, State};
handle_cast (_Request, State) ->
	{noreply, State}.

handle_info (Info, State) ->
	case Info of
		{'EXIT', _From, _Reason} ->
			ok;
		_Wtf ->
			ok
	end,
	{noreply, State}.

terminate (_Reason, _State) ->
	ok.

code_change (_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%
% External API %
%%%%%%%%%%%%%%%%
user(Username) when is_list(Username) ->
	gen_server:call(?MODULE, {user, Username}).

auth(Username, Password) when is_list(Username), is_list(Password) ->
	gen_server:call(?MODULE, {auth, Username, Password}).

admin(User, Username) when is_reference(User), is_list(Username) ->
	gen_server:call(?MODULE, {admin, User, Username}).

user_info(UserRef) when is_reference(UserRef) ->
	gen_server:call(?MODULE, {user_info, UserRef}).

delete_ref(UserRef) when is_reference(UserRef) ->
	gen_server:cast(?MODULE, {delete_ref, UserRef}).

%%%%%%%%%%%%%%%%%%%%
% Internal Helpers %
%%%%%%%%%%%%%%%%%%%%
ldap_attribute_val("ibutton", Val) ->
	Val;
ldap_attribute_val("drinkAdmin", Val) ->
	case hd(hd(Val)) of
		$1 -> true;
		$0 -> false
	end;
ldap_attribute_val("drinkBalance", Val) ->
	{Int, _Extra} = string:to_integer(hd(Val)),
	Int;
ldap_attribute_val(_Attr, Val) ->
	hd(Val).

ldap_attribute(Attr, {eldap_entry, _Dn, Attrs}) ->
	ldap_attribute(Attr, Attrs);
ldap_attribute(_Attr, []) ->
	false;
ldap_attribute(Attr, [{Name, ValueArr}|T]) ->
	case string:equal(Attr, Name) of
		true ->
			ldap_attribute_val(Attr, ValueArr);
		false ->
			ldap_attribute(Attr, T)
	end.

is_admin(UserRef, State) when is_reference(UserRef) ->
	case get_from_ref(UserRef, State) of
		{ok, UserInfo, Perms} ->
			lists:member(authed, Perms) and UserInfo#user.admin;
		{error, _Reason} ->
			false
	end.

respect_read_permissions(UserInfo, Result, [ibutton|T]) ->
	respect_read_permissions(UserInfo,
		Result#user{ibuttons=UserInfo#user.ibuttons}, T);
respect_read_permissions(UserInfo, Result, [read|T]) ->
	respect_read_permissions(UserInfo,
		Result#user{credits=UserInfo#user.credits,admin=UserInfo#user.admin}, T);
respect_read_permissions(UserInfo, Result, [_|T]) ->
	respect_read_permissions(UserInfo, Result, T);
respect_read_permissions(UserInfo, Result, []) ->
	Result#user{username=UserInfo#user.username}.

get_from_ref(UserRef, State) when is_reference(UserRef) ->
	case ets:lookup(State#uastate.reftable, UserRef) of
		[] ->
			{error, notfound};
		[{UserRef, Username, Perms}] ->
			case get_user(Username, State) of
				{ok, UserInfo} ->
					{ok, UserInfo, Perms};
				{error, Reason} ->
					{error, Reason}
			end
	end.

create_user_ref(Username, Perms, State) when is_list(Username), is_list(Perms) ->
	Ref = make_ref(),
	ets:insert(State#uastate.reftable, {Ref, Username, Perms}),
	Ref;
create_user_ref(Username, Perm, State) when is_list(Username), is_atom(Perm) ->
	create_user_ref(Username, [Perm], State);
create_user_ref(UserInfo, Perms, State) when is_tuple(UserInfo) ->
	create_user_ref(UserInfo#user.username, Perms, State).

get_ldap_user(Username, State) when is_list(Username) ->
	Base = {base, "ou=users,dc=csh,dc=rit,dc=edu"},
	Scope = {scope, eldap:singleLevel()},
	Filter = {filter, eldap:equalityMatch("uid", Username)},
	case eldap:search(State#uastate.ldapconn, [Base, Scope, Filter]) of
		{ok, {eldap_search_result, ResultList, []}} ->
			Admin = ldap_attribute("drinkAdmin", hd(ResultList)),
			Credits = ldap_attribute("drinkBalance", hd(ResultList)),
			IButtons = ldap_attribute("ibutton", hd(ResultList)),
			{ok, #user{username=Username,admin=Admin,credits=Credits,ibuttons=IButtons}};
		{error, Reason} ->
			{error, Reason};
		Reason ->
			{error, Reason}
	end.

get_user(Username, State) when is_list(Username) ->
	case ets:lookup(State#uastate.usertable, Username) of
		[] ->
			case get_ldap_user(Username, State) of
				{ok, UserInfo} ->
					ets:insert(State#uastate.usertable, UserInfo),
					{ok, UserInfo};
				{error, Reason} ->
					{error, Reason}
			end;
		[UserInfo] ->
			{ok, UserInfo}
	end.