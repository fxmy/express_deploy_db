-module(express_deploy_db).
-behavior(gen_server).

-export([update_db_by_file/1]).
-export([insert_new/3, get_by_ip/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  setup_db(),
  {ok, []}.

handle_call(Req, From, State) ->
  error_logger:warning_msg("unknown call: ~p from ~p, I'm ~p in state ~p~n", [Req, From, self(), State]),
  {noreply, State}.

handle_cast(Req, State) ->
  error_logger:warning_msg("uknown cast: ~p, I'm ~p in state ~p~n", [Req, self(), State]),
  {noreply, State}.

handle_info(Info, State) ->
  error_logger:warning_msg("uknown info: ~p, I'm ~p in state ~p~n", [Info, self(), State]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

-spec setup_db() -> ok.
setup_db() ->
  % boilerplate code for setting up sumo_db
  Node = node(),
  _ = application:stop(mnesia),
  case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  ok = application:start(mnesia),
  sumo:create_schema(),

  application:ensure_all_started(sumo_db),
  sumo:create_schema(express_account).

-spec update_db_by_file(string()) -> file_notfound | ok.
update_db_by_file(FileName) ->
  express_deploy_db_csv:update_db_by_file(FileName).

-spec insert_new(express_deploy_db_account:ip(),
                 express_deploy_db_account:user_name(),
                 express_deploy_db_account:password()) ->
  express_deploy_db_account:account().
insert_new(Ip, UserName, PassWd) ->
  Account = express_deploy_db_account:new(Ip, UserName, PassWd),
  sumo:persist(express_account, Account).

-spec get_by_ip(express_deploy_db_account:ip()) ->
  {express_deploy_db_account:user_name(),
   express_deploy_db_account:password()}
  | notfound.
get_by_ip(Ip) ->
  case sumo:fetch(express_account, Ip) of
    notfound -> notfound;
    Account ->
      {express_deploy_db_account:user_name(Account),
       express_deploy_db_account:password(Account)}
  end.
