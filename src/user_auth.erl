%%%-------------------------------------------------------------------
%%% File    : user_auth.erl
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

-module (user_auth).
-behaviour (gen_server).

-export ([start_link/1]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([user/1, user/2, auth/1, auth/2, admin/2, can_admin/1, can_admin_noblock/1, 
          user_info/1, user_info_noblock/1, delete_ref/1, drop/3, add_credits/3, 
          dec_credits/3, mod_credits/3, set_admin/2, add_ibutton/2, del_ibutton/2]).

-include ("user.hrl").
-include ("drink_mnesia.hrl").
-include_lib ("drink_log/include/drink_log.hrl").

-record (uastate, {userinfo_mod}).

%%%%%%%%%%%%%%%%%%%%%%%%
% Gen_Server Callbacks %
%%%%%%%%%%%%%%%%%%%%%%%%
start_link (UserInfoMod) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, UserInfoMod, []).

init (UserInfoMod) ->
	process_flag(trap_exit, true),
	ets:new(ref2user, [set, protected, named_table]),
	ets:new(userinfo, [set, protected, {keypos, 2}, named_table]),
	ok = UserInfoMod:init(),
	{ok, #uastate{
	    userinfo_mod = UserInfoMod}}.

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
handle_call ({auth, {webauth, Username}}, _From, State) when is_list(Username) ->
    {reply, {ok, create_user_ref(Username, [read, drop, authed], State)}, State};
handle_call ({admin, Admin, Username}, _From, State) when is_reference(Admin), is_list(Username) ->
	case get_user(Username, State) of
		{ok, User} ->
			case is_admin(Admin) of
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
    {reply, is_admin(UserRef), State};
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
					case {drink_machine:is_alive(Machine), drink_machine:slot_info(Machine, Slot)} of
						{true, {ok, SlotInfo}} when SlotInfo#slot.avail > 0 ->
							Result = drop_slot(UserInfo, Machine, Slot, State),
							{reply, Result, State};
						{true, {ok, _X}} ->
							{reply, {error, slot_empty}, State};
						{false, _} ->
						    {reply, {error, machine_down}, State};
						{_, {error, Reason}} ->
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
handle_call ({add_ibutton, UserRef, IButton}, _From, State) when is_reference(UserRef), is_list(IButton) ->
    case get_from_ref(UserRef, State) of
        {ok, UserInfo, Perms} ->
            case can_write(Perms) of
                true ->
                    case user_add_ibutton(UserInfo, IButton, State) of
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
handle_call ({del_ibutton, UserRef, IButton}, _From, State) when is_reference(UserRef), is_list(IButton) ->
    case get_from_ref(UserRef, State) of
        {ok, UserInfo, Perms} ->
            case can_write(Perms) of
                true ->
                    case user_del_ibutton(UserInfo, IButton, State) of
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
	ets:delete(ref2user, UserRef),
	{noreply, State};
handle_cast (_Request, State) ->
	{noreply, State}.

handle_info ({'EXIT', _From, _Reason}, State) ->
    {noreply, State};
handle_info (_Info, State) ->
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
	gen_server:call(?MODULE, {auth, Ibutton});
auth({webauth, Username}) when is_list(Username) ->
    gen_server:call(?MODULE, {auth, {webauth, Username}}).

admin(User, Username) when is_reference(User), is_list(Username) ->
	gen_server:call(?MODULE, {admin, User, Username}).

user_info(UserRef) when is_reference(UserRef) ->
	gen_server:call(?MODULE, {user_info, UserRef}).

user_info_noblock(UserRef) when is_reference(UserRef) ->
 	case get_from_ref(UserRef, undefined) of
		{ok, UserInfo, Perms} ->
			{ok, respect_read_permissions(UserInfo, #user{}, Perms)};
		{error, Reason} ->
			{error, Reason}
	end.

can_admin(nil) ->
    false;
can_admin(UserRef) when is_reference(UserRef) ->
    gen_server:call(?MODULE, {can_admin, UserRef}).

can_admin_noblock(UserRef) when is_reference(UserRef) ->
    is_admin(UserRef).

delete_ref(UserRef) when is_reference(UserRef) ->
	gen_server:cast(?MODULE, {delete_ref, UserRef}).

drop(UserRef, Machine, Slot) when is_reference(UserRef), is_atom(Machine), is_integer(Slot) ->
	gen_server:call(?MODULE, {drop, UserRef, Machine, Slot}, infinity).

add_credits(UserRef, Credits, Reason) when is_reference(UserRef), is_integer(Credits) ->
    gen_server:call(?MODULE, {add_credits, UserRef, Credits, Reason}).

dec_credits(UserRef, Credits, Reason) when is_reference(UserRef), is_integer(Credits) ->
    gen_server:call(?MODULE, {dec_credits, UserRef, Credits, Reason}).

mod_credits(UserRef, Credits, Reason) when is_integer(Credits), Credits > 0 ->
    add_credits(UserRef, Credits, Reason);
mod_credits(UserRef, Credits, Reason) when is_integer(Credits), Credits < 0 ->
    dec_credits(UserRef, 0 - Credits, Reason).

set_admin(UserRef, Admin) when is_reference(UserRef), is_atom(Admin) ->
    gen_server:call(?MODULE, {set_admin, UserRef, Admin}).

add_ibutton(UserRef, IButton) when is_reference(UserRef), is_list(IButton) ->
    gen_server:call(?MODULE, {add_ibutton, UserRef, IButton}).

del_ibutton(UserRef, IButton) when is_reference(UserRef), is_list(IButton) ->
    gen_server:call(?MODULE, {del_ibutton, UserRef, IButton}).

%%%%%%%%%%%%%%%%%%%%
% Internal Helpers %
%%%%%%%%%%%%%%%%%%%%
valid_admin_setting(true) -> ok;
valid_admin_setting(false) -> ok;
valid_admin_setting(_) -> {error, invalid_arg}.

valid_ibutton_character(Char) when Char =< $F, Char >= $A ->
    true;
valid_ibutton_character(Char) when Char =< $9, Char >= $0 ->
    true;
valid_ibutton_character(_) ->
    false.

valid_ibutton(IButton) when is_list(IButton), length(IButton) =:= 16 ->
    case lists:any(fun valid_ibutton_character/1, IButton) of
        true ->
            ok;
        false ->
            {error, bad_ibutton}
    end;
valid_ibutton(_) ->
    {error, bad_ibutton}.

deduct(UserInfo, Cost, MoneyReason, State) when is_tuple(UserInfo) ->
	case get_user(UserInfo#user.username, State) of
		{ok, User} ->
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
					case catch (State#uastate.userinfo_mod):set_credits(NewUserInfo) of
					    ok ->
        					ets:insert(userinfo, NewUserInfo),
                            dw_events:send(drink, MoneyLog),
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
			    admin = UserInfo#user.adminuser,
			    direction = in
			},
			case catch (State#uastate.userinfo_mod):set_credits(NewUserInfo) of
			    ok ->
        			ets:insert(userinfo, NewUserInfo),
                    dw_events:send(drink, MoneyLog),
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
                    case catch (State#uastate.userinfo_mod):set_admin(NewUserInfo) of
                        ok ->
                            ets:insert(userinfo, NewUserInfo),
                            dw_events:send(drink, {user_changed, User#user.username, [{admin, User#user.admin, NewUserInfo#user.admin}]}),
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

user_add_ibutton(UserInfo, IButton, State) when is_tuple(UserInfo) ->
    case valid_ibutton(IButton) of
        ok ->
            case get_user(UserInfo#user.username, State) of
                {ok, User} ->
                    NewUserInfo = User#user{ibuttons=User#user.ibuttons ++ [IButton]},
                    case catch (State#uastate.userinfo_mod):add_ibutton(NewUserInfo#user.username, IButton) of
                        ok ->
                            ets:insert(userinfo, NewUserInfo),
                            dw_events:send(drink, {user_changed, User#user.username, [{add_ibutton, IButton}]}),
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

user_del_ibutton(UserInfo, IButton, State) when is_tuple(UserInfo) ->
    case get_user(UserInfo#user.username, State) of
        {ok, User} ->
            case lists:member(IButton, User#user.ibuttons) of
                true ->
                    NewUserInfo = User#user{ibuttons=User#user.ibuttons -- [IButton]},
                    case catch (State#uastate.userinfo_mod):del_ibutton(NewUserInfo#user.username, IButton) of
                        ok ->
                            ets:insert(userinfo, NewUserInfo),
                            dw_events:send(drink, {user_changed, User#user.username, [{del_ibutton, IButton}]}),
                            {ok, NewUserInfo};
                        {error, Reason} ->
                            {error, Reason};
                        Reason ->
                            {error, Reason}
                    end;
                false ->
                    {error, no_ibutton_for_user}
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
		        slot = SlotInfo#slot.name,
		        username = UserInfo#user.username,
		        time = erlang:universaltime()
		    },
			case drink_machine:drop(Machine, Slot) of
				{ok} ->
                    dw_events:send(drink, DropLog),
					ok;
				{error, Reason} ->
					refund(NewUserInfo, SlotInfo#slot.price, drop_error, State),
                    dw_events:send(drink, DropLog#drop_log{status={error, Reason}}),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

can_drop(Perms) ->
    lists:member(drop, Perms).

can_write(Perms) ->
    lists:member(write, Perms).

% For times when we don't have the State variable
% This may return false if we don't have the row in the user info cache
is_admin(UserRef) when is_reference(UserRef) -> is_admin(UserRef, undefined).

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
	case ets:lookup(ref2user, UserRef) of
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

create_user_ref(Username, Perms, Admin, _State) when is_list(Username), is_list(Perms) ->
	Ref = make_ref(),
	ets:insert(ref2user, {Ref, Username, Perms, Admin}),
	Ref;
create_user_ref(Username, Perm, Admin, State) when is_list(Username), is_atom(Perm) ->
	create_user_ref(Username, [Perm], Admin, State);
create_user_ref(UserInfo, Perms, Admin, State) when is_tuple(UserInfo) ->
	create_user_ref(UserInfo#user.username, Perms, Admin, State).
	
create_user_ref(User, Perms, State) ->
    create_user_ref(User, Perms, nil, State).

get_user(Username, State) when is_list(Username) ->
	case {State, ets:lookup(userinfo, Username)} of
		{undefined, []} -> {error, state_undefined};
        {_, []}->
			case (State#uastate.userinfo_mod):get_user(username, Username) of
				{ok, UserInfo} ->
					ets:insert(userinfo, UserInfo),
					{ok, UserInfo};
				{error, Reason} ->
					{error, Reason}
			end;
		{_, [UserInfo]} ->
			{ok, UserInfo}
	end.

get_user_from_ibutton(Ibutton, State) when is_list(Ibutton) ->
    case (State#uastate.userinfo_mod):get_user(ibutton, Ibutton) of
        {ok, UserInfo} ->
            ets:insert(userinfo, UserInfo),
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

