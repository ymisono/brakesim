%%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(throttle).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0,start_debug/0]).

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
	init(),
	event_loop(false).

start_debug() ->
	init(),
	event_loop(debug).
%% ====================================================================
%% Internal functions
%% ====================================================================
init()->
	SelfPid = self(),
	process_flag(trap_exit, true),
	Pid =  spawn_link(fun() -> throttle_gui:start(SelfPid) end),
	put(child_gui, Pid).

event_loop(Option) ->
	receive
		{state_inf, chgui,_Old,Parameters} when Option =:= debug->
			io:format("throttle receive state_inf from childGui Para= ~p~n", [Parameters]),
			event_loop(Option);
		{state_inf, chgui,Old,Parameters} ->
			CrePara = {Old,Parameters},
			gen_server:cast({global,transmission},{state_inf, throttle,crePara(CrePara)}),
			gen_server:cast({global,dcs},{state_inf, throttle,crePara(CrePara)}),
			gen_server:cast({global,output},{state_inf, throttle,crePara(CrePara)}),
			%% save
			gen_server:cast({global,sim_db},{msgsav,throttle,{state_inf, throttle,Old,Parameters}}),
			event_loop(Option);

		{state_inf,_,Parameters} ->
			Toru = getParam(torque,Parameters),
			get(child_gui)! {state_inf,parent,Toru},
			event_loop(Option);

		{'EXIT', Pid, Why} ->
			io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);
		_Any ->
%%			io:format("unknown type received : ~p~n", [_Any]),
			event_loop(Option)
	end.
	
%%-------------------------------------
%% tool
%%-------------------------------------
%%get parameter from message------
getParam(Key,[Hk,Hv|_]) when Key =:= Hk->
	Hv;
getParam(Key,[_,_|T])->
	getParam(Key,T);
getParam(_,[])->
	false.

crePara({Old,Torque})->
	if
		Old =< Torque ->[opetype,throttle,userope,speedup,torque,Torque];
		true->		[opetype,throttle,userope,speeddown,torque,Torque]
	end.
