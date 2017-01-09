-module(express_deploy_db_csv).
-export([update_db_by_file/1]).

-spec update_db_by_file(string()) -> file_notfound | ok.
update_db_by_file(FileName) when is_list(FileName) ->
  case file:open(FileName, [read]) of
    {ok, IoDevice} ->
      update_db(IoDevice);
    {error, Why} ->
      error_logger:warning_msg("Open csv file: ~p failed with reason ~p~n", [FileName, Why]),
      file_notfound
  end.

-spec update_db(file:io_device()) -> ok.
update_db(IoDevice) ->
  case io:get_line(IoDevice, "") of
    eof ->
      _ = file:close(IoDevice),
      ok;
    {error, Why} ->
      error_logger:warning_msg("Get line error in device: ~p with reason ~p~n", [IoDevice, Why]),
      _ = file:close(IoDevice),
      ok;
    DataLine ->
      [Ip, UserName, PassWd] = parse_csv_line(DataLine),
      express_deploy_db:insert_new(Ip, UserName, trim_crlf(PassWd)),
      update_db(IoDevice)
  end.

-spec trim_crlf(string()) -> string().
trim_crlf(String) ->
  trim_crlf(String, []).

trim_crlf([], Acc) ->
  lists:reverse(Acc);
trim_crlf([$\n|T], Acc) ->
  trim_crlf(T, Acc);
trim_crlf([H|T], Acc) ->
  trim_crlf(T, [H|Acc]).
%%==================================
%% Internal
%%==================================
-spec parse_csv_line(string()) -> [string()].
parse_csv_line(String) -> parse_csv_line(String, [], [], [], false).

parse_csv_line([], S, Acc, [], _) -> 
  lists:reverse(
    lists:map(
      fun(X) ->
          lists:reverse(lists:flatten(X)) end, [Acc|S]
     )
   );
parse_csv_line([], S, [], L, _) ->
  lists:reverse(
    lists:map(
      fun(X) ->
          lists:reverse(lists:flatten(X)) end, [L|S]
     )
   );
parse_csv_line(String, S, Acc, L, IsSubStr) ->
  case String of
    [$"|T] when IsSubStr =:= true ->
      % end of substring (ending quote).
      parse_csv_line(T, S, Acc, [$"|L], false);
    [$"|T] when IsSubStr =:= false ->
      % beginning of a substring (beginning quote).
      parse_csv_line(T, S, Acc, [$"], true);
    [$,|T] when IsSubStr =:= true andalso L =/= [] ->
      % comma within a substring
      parse_csv_line(T, S, Acc, [$,|L], true);
    [$,|T] when IsSubStr =:= false andalso L =/= [] ->
      % comma after a substring.
      parse_csv_line(T, [[L|Acc]|S], [], [], false);
    [$,|T] when IsSubStr =:= false andalso L =:= [] ->
      % comma after a normal string.
      parse_csv_line(T, [Acc|S], [], [], false);
    [H|T] when IsSubStr =:= true ->
      % within a substring
      parse_csv_line(T, S, Acc, [H|L], true);
    [H|T] when IsSubStr =:= false ->
      % a normal string
      parse_csv_line(T, S, [H|Acc], [], false) end.
