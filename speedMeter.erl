%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/10/11
%%% -------------------------------------------------------------------
-module(speedMeter).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0,sendmsg/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(REGNAME, speed).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
-compile(export_all).


start() ->
  process_flag(trap_exit, true),
  case whereis(?REGNAME) of
    undefined -> 
      Pid = spawn(?MODULE, receiver_main, []),
      register(?REGNAME, Pid);

    _-> true
  end.


receiver_main() ->
  put(speed,0),
  receiver_main2().

receiver_main2() ->
  receive
   {'EXIT', Pid, Why} ->
     io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
     exit(error);

   {msgsend, Speed} -> 
      case get(pid) of
        undefined -> false;
        _->
            get(pid)! {state_inf,paroutput,speed,Speed}
      end;

   {message, Pid, Message} -> % データ受信
     io:fwrite("Received! ===> From:~w Message:~s~n", [Pid, Message]),
     put(pid,Pid),
     Pid! {state_inf,paroutput,speed,0};

   Other -> % それ以外
      io:fwrite("Oops! ~w is ignored.\n", [Other])

  end,
  receiver_main2().


sendmsg(Speed)->
  case whereis(?REGNAME) of
    undefined -> false;
    _->
        ?REGNAME!{msgsend,Speed}
  end.



