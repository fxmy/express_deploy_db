%%%-------------------------------------------------------------------
%% @doc express_deploy_db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(express_deploy_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 0, 1}, [child()]} }.

%%====================================================================
%% Internal functions
%%====================================================================
child() ->
  {express_deploy_db, {express_deploy_db, start_link, []},
   permanent, 5000, worker, [express_deploy_db]}.
