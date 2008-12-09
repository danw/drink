-module (user_auth).
-behaviour (gen_server).

-export ([start_link/0]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([user/1, user/2, auth/1, auth/2, admin/2, can_admin/1, user_info/1, delete_ref/1, drop/3, add_credits/3, dec_credits/3, set_admin/2]).

% for testing:
-export ([ldap_set_credits/1]).

-include ("user.hrl").
-include ("drink_mnesia.hrl").

-record (uastate, {reftable,usertable}).

%%%%%%%%%%%%%%%%%%%%%%%%
% Gen_Server Callbacks %
%%%%%%%%%%%%%%%%%%%%%%%%
start_link () ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init ([]) ->
	process_flag(trap_exit, true),
	RefTable = ets:new(ref2user, [set, private]),
	UserTable = ets:new(userinfo, [set, private, {keypos, 2}]),
	{ok, #uastate{
	    reftable = RefTable,
            usertable = UserTable}}.

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
			case check_pass(Username, Password) of
				true ->
					{reply, {ok, create_user_ref(User, [read, drop, authed], State)}, State};
				false ->
					{reply, {error, badpass}, State}
			end;
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({auth, Ibutton}, _From, State) when is_list(Ibutton) ->
	case get_user_from_ibutton(Ibutton, State) of
		{ok, User} ->
			{reply, {ok, create_user_ref(User, [read, drop, authed], State)}, State};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({admin, Admin, Username}, _From, State) when is_reference(Admin), is_list(Username) ->
	case get_user(Username, State) of
		{ok, User} ->
			case is_admin(Admin, State) of
				true ->
				    {ok, AdminUser, _Perms} = get_from_ref(Admin, State),
					{reply, {ok, create_user_ref(User, [read, write, ibutton], AdminUser#user.username, State)}, State};
				false ->
					{reply, {error, permission_denied}, State}
			end;
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({can_admin, UserRef}, _From, State) when is_reference(UserRef) ->
    {reply, is_admin(UserRef, State), State};
handle_call ({user_info, UserRef}, _From, State) when is_reference(UserRef) ->
	case get_from_ref(UserRef, State) of
		{ok, UserInfo, Perms} ->
			{reply, {ok, respect_read_permissions(UserInfo, #user{}, Perms)}, State};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({drop, UserRef, Machine, Slot}, _From, State) when is_reference(UserRef),
																is_atom(Machine), is_integer(Slot) ->
	case get_from_ref(UserRef, State) of
		{ok, UserInfo, Perms} ->
			case can_drop(Perms) of
				true -> 
					case drink_machine:slot_info(Machine, Slot) of
						{ok, SlotInfo} when SlotInfo#slot.avail > 0 ->
							Result = drop_slot(UserInfo, Machine, Slot, State),
							{reply, Result, State};
						{ok, _X} ->
							{reply, {error, slot_empty}, State};
						{error, Reason} ->
							{reply, {error, Reason}, State}
					end;
				false ->
					{reply, {error, permission_denied}, State}
			end;
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({add_credits, UserRef, Credits, CallReason}, _From, State) when is_reference(UserRef), is_integer(Credits) ->
    case get_from_ref(UserRef, State) of
        {ok, UserInfo, Perms} ->
            case can_write(Perms) of
                true ->
                    case refund(UserInfo, Credits, CallReason, State) of
                        {ok, _NewUserInfo} ->
                            {reply, ok, State};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                false ->
                    {reply, {error, permission_denied}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call ({dec_credits, UserRef, Credits, CallReason}, _From, State) when is_reference(UserRef), is_integer(Credits) ->
    case get_from_ref(UserRef, State) of
        {ok, UserInfo, Perms} ->
            case can_write(Perms) of
                true ->
                    case deduct(UserInfo, Credits, CallReason, State) of
                        {ok, _NewUserInfo} ->
                            {reply, ok, State};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                false ->
                    {reply, {error, permission_denied}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call ({set_admin, UserRef, Admin}, _From, State) when is_reference(UserRef), is_atom(Admin) ->
    case get_from_ref(UserRef, State) of
        {ok, UserInfo, Perms} ->
            case can_write(Perms) of
                true ->
                    case user_set_admin(UserInfo, Admin, State) of
                        {ok, _NewUserInfo} ->
                            {reply, ok, State};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                false ->
                    {reply, {error, permission_denied}, State}
            end;
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
	io:format("Got codechange!~n"),
	{ok, State}.

%%%%%%%%%%%%%%%%
% External API %
%%%%%%%%%%%%%%%%
user(nil, Username) ->
    user(Username);
user(AdminUser, Username) ->
    case can_admin(AdminUser) of
        true ->
            admin(AdminUser, Username);
        _false ->
            user(Username)
    end.
user(Username) when is_list(Username) ->
	gen_server:call(?MODULE, {user, Username}).

auth(Username, Password) when is_list(Username), is_list(Password) ->
	gen_server:call(?MODULE, {auth, Username, Password}).

auth(Ibutton) when is_list(Ibutton) ->
	gen_server:call(?MODULE, {auth, Ibutton}).

admin(User, Username) when is_reference(User), is_list(Username) ->
	gen_server:call(?MODULE, {admin, User, Username}).

user_info(UserRef) when is_reference(UserRef) ->
	gen_server:call(?MODULE, {user_info, UserRef}).

can_admin(UserRef) when is_reference(UserRef) ->
    gen_server:call(?MODULE, {can_admin, UserRef}).

delete_ref(UserRef) when is_reference(UserRef) ->
	gen_server:cast(?MODULE, {delete_ref, UserRef}).

drop(UserRef, Machine, Slot) when is_reference(UserRef), is_atom(Machine), is_integer(Slot) ->
	gen_server:call(?MODULE, {drop, UserRef, Machine, Slot}).

add_credits(UserRef, Credits, Reason) when is_reference(UserRef), is_integer(Credits) ->
    gen_server:call(?MODULE, {add_credits, UserRef, Credits, Reason}).

dec_credits(UserRef, Credits, Reason) when is_reference(UserRef), is_integer(Credits) ->
    gen_server:call(?MODULE, {dec_credits, UserRef, Credits, Reason}).

set_admin(UserRef, Admin) when is_reference(UserRef), is_atom(Admin) ->
    gen_server:call(?MODULE, {set_admin, UserRef, Admin}).

%%%%%%%%%%%%%%%%%%%%
% Internal Helpers %
%%%%%%%%%%%%%%%%%%%%
valid_admin_setting(true) -> ok;
valid_admin_setting(false) -> ok;
valid_admin_setting(_) -> {error, invalid_arg}.

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

val_to_ldap_attr(admin, Val) ->
    case Val of
        true ->
            "1";
        false ->
            "0"
    end.

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

deduct(UserInfo, Cost, MoneyReason, State) when is_tuple(UserInfo) ->
	case get_user(UserInfo#user.username, State) of
		{ok, User} ->
			io:format("Deducting ~p from ~p(~p)~n", [Cost, User#user.username, User#user.credits]),
			case User#user.credits of
				B when B >= Cost ->
					NewUserInfo = User#user{credits = B - Cost},
					MoneyLog = #money_log{
					    time = erlang:universaltime(),
					    username = UserInfo#user.username,
					    amount = 0 - Cost,
					    reason = MoneyReason,
					    admin = UserInfo#user.adminuser
					},
					case catch ldap_set_credits(NewUserInfo) of
					    ok ->
        					ets:insert(State#uastate.usertable, NewUserInfo),
        					drink_mnesia:log_money(MoneyLog),
        					{ok, NewUserInfo};
        				{error, Reason} ->
        				    error_logger:error_msg("Failed to deduct ~b credits from ~s: ~p~n", [Cost, UserInfo#user.username, Reason]),
                		    {error, Reason};
        				Reason ->
                		    error_logger:error_msg("Failed to deduct ~b credits from ~s: ~p~n", [Cost, UserInfo#user.username, Reason]),
        				    {error, Reason}
        			end;
				_ ->
					{error, poor}
			end;
		{error, Reason} ->
		    error_logger:error_msg("Failed to deduct ~b credits from ~s: ~p~n", [Cost, UserInfo#user.username, Reason]),
			{error, Reason}
	end.

refund(UserInfo, Amount, MoneyReason, State) when is_tuple(UserInfo) ->
	case get_user(UserInfo#user.username, State) of
		{ok, User} ->
			NewUserInfo = User#user{credits = User#user.credits + Amount},
			MoneyLog = #money_log{
			    time = erlang:universaltime(),
			    username = UserInfo#user.username,
			    amount = Amount,
			    reason = MoneyReason,
			    admin = User#user.adminuser,
			    direction = in
			},
			case catch ldap_set_credits(NewUserInfo) of
			    ok ->
        			ets:insert(State#uastate.usertable, NewUserInfo),
        			drink_mnesia:log_money(MoneyLog),
        			{ok, NewUserInfo};
        		{error, Reason} ->
	    			error_logger:error_msg("Failed to refund ~s ~b credits: ~p~n", [UserInfo#user.username, Amount, Reason]),
        		    {error, Reason};
        		Reason ->
	    			error_logger:error_msg("Failed to refund ~s ~b credits: ~p~n", [UserInfo#user.username, Amount, Reason]),
        		    {error, Reason}
        	end;
		{error, Reason} ->
			error_logger:error_msg("Failed to refund ~s ~b credits: ~p~n", [UserInfo#user.username, Amount, Reason]),
			{error, Reason}
	end.

user_set_admin(UserInfo, Admin, State) when is_tuple(UserInfo) ->
    case valid_admin_setting(Admin) of
        ok ->
            case get_user(UserInfo#user.username, State) of
                {ok, User} ->
                    NewUserInfo = User#user{admin = Admin},
                    case catch ldap_set_admin(NewUserInfo) of
                        ok ->
                            ets:insert(State#uastate.usertable, NewUserInfo),
                            {ok, NewUserInfo};
                        {error, Reason} ->
                            {error, Reason};
                        Reason ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

drop_slot(UserInfo, Machine, Slot, State) when is_tuple(UserInfo), is_atom(Machine), is_integer(Slot) ->
	{ok, SlotInfo} = drink_machine:slot_info(Machine, Slot),
	case deduct(UserInfo, SlotInfo#slot.price, drop, State) of
		{ok, NewUserInfo} ->
		    DropLog = #drop_log{
		        machine = Machine,
		        slot = Slot,
		        username = UserInfo#user.username,
		        time = erlang:universaltime()
		    },
			case drink_machine:drop(Machine, Slot) of
				{ok} ->
				    drink_mnesia:log_drop(DropLog),
					ok;
				{error, Reason} ->
					refund(NewUserInfo, SlotInfo#slot.price, drop_error, State),
					drink_mnesia:log_drop(DropLog#drop_log{status={error, Reason}}),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

can_drop(Perms) ->
    lists:member(drop, Perms).

can_write(Perms) ->
    lists:member(write, Perms).

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
			{error, invalid_ref};
		[{UserRef, Username, Perms, Admin}] ->
			case get_user(Username, State) of
				{ok, UserInfo} ->
					{ok, UserInfo#user{adminuser = Admin}, Perms};
				{error, Reason} ->
					{error, Reason}
			end
	end.

create_user_ref(Username, Perms, Admin, State) when is_list(Username), is_list(Perms) ->
	Ref = make_ref(),
	ets:insert(State#uastate.reftable, {Ref, Username, Perms, Admin}),
	Ref;
create_user_ref(Username, Perm, Admin, State) when is_list(Username), is_atom(Perm) ->
	create_user_ref(Username, [Perm], Admin, State);
create_user_ref(UserInfo, Perms, Admin, State) when is_tuple(UserInfo) ->
	create_user_ref(UserInfo#user.username, Perms, Admin, State).
	
create_user_ref(User, Perms, State) ->
    create_user_ref(User, Perms, nil, State).

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

get_user(Username, State) when is_list(Username) ->
	case ets:lookup(State#uastate.usertable, Username) of
		[] ->
			case get_ldap_user("uid", Username) of
				{ok, UserInfo} ->
					ets:insert(State#uastate.usertable, UserInfo),
					{ok, UserInfo};
				{error, Reason} ->
					{error, Reason}
			end;
		[UserInfo] ->
			{ok, UserInfo}
	end.

get_user_from_ibutton(Ibutton, State) when is_list(Ibutton) ->
    case get_ldap_user("ibutton", Ibutton) of
        {ok, UserInfo} ->
            ets:insert(State#uastate.usertable, UserInfo),
            {ok, UserInfo};
        {error, Reason} ->
            {error, Reason}
    end.

check_pass(User, Pass) ->
    case epam:authenticate("drink", User, Pass) of
        true ->
            true;
        _Else ->
            false
    end.

ldap_set_credits(UserInfo) ->
    eldap:modify(eldap_user, 
        "uid=" ++ UserInfo#user.username ++ ",ou=users,dc=csh,dc=rit,dc=edu",
        [eldap:mod_replace("drinkBalance", [integer_to_list(UserInfo#user.credits)])]).

ldap_set_admin(UserInfo) ->
    eldap:modify(eldap_user,
        "uid=" ++ UserInfo#user.username ++ ",ou=users,dc=csh,dc=rit,dc=edu",
        [eldap:mod_replace("drinkAdmin", [val_to_ldap_attr(admin, UserInfo#user.admin)])]).