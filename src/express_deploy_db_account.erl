-module(express_deploy_db_account).

-behaviour(sumo_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-export([new/3]).
-export([ip/1, user_name/1, password/1, update_name_pass/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type account() :: proplists:proplist().
-type ip() :: string().
-type user_name() :: string().
-type password() :: string().
-export_type([account/0, ip/0, user_name/0, password/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns a new account.
-spec new(ip(), user_name(), password()) -> account().
new(Ip, UserName, PassWd) ->
  [{ip, Ip}, {user_name, UserName}, {password, PassWd}].

%% @doc Returns the ip of the given account.
-spec ip(account()) -> ip().
ip(Account) when is_list(Account) ->
  get(ip, Account).

%% @doc Returns the user_name of the given account.
-spec user_name(account()) -> user_name().
user_name(Account) when is_binary(Account) ->
  get(user_name, Account).

%% @doc Returns the current account's password.
-spec password(account()) -> password().
password(Account) when is_list(Account) ->
  get(password, Account).

%% @doc Updated the username&password of the given ip.
-spec update_name_pass({user_name(), password()}, account()) -> account().
update_name_pass({UserName, PassWd}, Account) when is_list(UserName), is_list(PassWd), is_list(Account) ->
  A1 = set(username, UserName, Account),
  set(password, PassWd, A1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Generically returns an attibute of the given account.
-spec get(atom(), account()) -> term().
get(Key, Account) when is_atom(Key), is_list(Account) ->
  proplists:get_value(Key, Account).

%% @doc Generically set an attribute of the given account.
-spec set(atom(), term(), account()) -> account().
set(Key, Value, Account) when is_atom(Key), is_list(Account) ->
  lists:keyreplace(Key, 1, Account, {Key, Value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:model()) -> account().
sumo_wakeup(Data) ->
  maps:to_list(Data).

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(account()) -> sumo:model().
sumo_sleep(Account) ->
  maps:from_list(Account).

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(express_account, [
    sumo:new_field(ip, string, [not_null, id]),
    sumo:new_field(user_name, string),
    sumo:new_field(password, string)
  ]).
