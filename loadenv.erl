%%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/21
%%% -------------------------------------------------------------------
-module(loadenv).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0,calDynFrictional/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
	SelfPid = self(),
	process_flag(trap_exit, true),
	Pid =  spawn_link(fun() -> loadenv_gui:start(SelfPid) end),

	put(child_gui, Pid),
%-----patch------------
	sim_db:add_Caldat(area,1.9),%v2
	sim_db:add_loavenv_item(wheelFright,0.7,calDynFrictional(0.7),1000),
	sim_db:add_loavenv_item(wheelFleft,0.7,calDynFrictional(0.7),1000),
	sim_db:add_loavenv_item(wheelRright,0.7,calDynFrictional(0.7),1000),
	sim_db:add_loavenv_item(wheelRleft,0.7,calDynFrictional(0.7),1000),
%-----------------
	event_loop().


%% ====================================================================
%% Internal functions
%% ====================================================================
event_loop() ->
	receive
		{state_inf, chgui,[frictional,Val]} ->
			sim_db:add_loavenv_item(wheelFright,Val,calDynFrictional(Val),1000),
			sim_db:add_loavenv_item(wheelFleft,Val,calDynFrictional(Val),1000),
			sim_db:add_loavenv_item(wheelRright,Val,calDynFrictional(Val),1000),
			sim_db:add_loavenv_item(wheelRleft,Val,calDynFrictional(Val),1000),
			event_loop();

		{state_inf, sim_db,[area,Val]} ->
			get(child_gui)!{state_inf,pare,[area,Val]},
			event_loop();

		{'EXIT', _Pid, normal} ->
			bye;
		{'EXIT', Pid, Why} ->
			io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);
		_Any ->
			io:format("loadenv unknown type received : ~p~n", [_Any]),
			event_loop()
	end.

%% ====================================================================
%% Intra functions
%% ====================================================================
calDynFrictional(Sta)->
	Sta * 2/ 3.

