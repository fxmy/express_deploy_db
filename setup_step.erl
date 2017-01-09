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

%% need to change ram_copy to disc_copy
