-module(express_deploy_db).

-export([setup_db/0, update_db_by_file/1]).
-export([insert_new/3, get_by_ip/1]).

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
