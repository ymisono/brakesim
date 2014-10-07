%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(dcs).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(cruisectr,[start/1]).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0]).

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
	io:format("-------------dcs Start node=~p--------------~n",[ erlang:node()]),
	process_flag(trap_exit, true),
	event_loop(),
	put(slipmod,off),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

event_loop() ->
	receive
		{state_inf,frsensor, Parameter} ->
			io:format("frsensor=~p~n",[Parameter]),
			_Des = getParam(distance,Parameter),
			event_loop();
		{state_inf,bksensor, Parameter} ->
			io:format("bksensor=~p~n",[Parameter]),
			case getParam(distance,Parameter) of
				Des when Des >0 ->
					put(slipmod,on);
				_->
					false
			end,
			event_loop();
		{state_inf,risensor, Parameter} ->
			io:format("risensor=~p~n",[Parameter]),
			_Des = getParam(distance,Parameter),
			event_loop();
		{state_inf,lfsensor, Parameter} ->
			io:format("lfsensor=~p~n",[Parameter]),
			_Des = getParam(distance,Parameter),
			event_loop();
		{state_inf,throttle, Parameter} ->
			%演習2-1
			%gen_server:cast({global,transmission},{update_ind,dcs,[opetype,throttle,userope,speedup,torque,10000]}),
			io:format("throttle=~p~n",[Parameter]),
			%演習2-4
			put(torque,getParam(torque,Parameter)),
			io:format("torque=~p~n",[get(torque)]),
			event_loop();

		{state_inf,brake, _Parameter} ->
			io:format("brake=~p~n",[_Parameter]),
			event_loop();

		{state_inf,wheelFright, Parameter} ->
			%演習1-3
			RPM = getParam(rpm,Parameter),
			io:format("FR's rpm=~p~n",[RPM]),
			%演習1-4
			[TireRadius] = sim_db:getDbValue(caldata,tyre),
			%演習2-2
			Speed = round(RPM * 60 * TireRadius/1000),
			put(speed,Speed),
			io:format("FR's speed=~p~n[km/h]",[Speed]),
			%演習2-3
			if
				Speed < 130 ->
					io:format("cruise control: speeding up...~n"),
					gen_server:cast({global,transmission},{update_ind,dcs,[opetype,throttle,userope,speedup,torque,45000]});

				Speed >= 130 -> 
					io:format("WARNING!: You're going too fast~n"),
					io:format("cruise control: speeding down~n"),
					%演習2-5（スピードダウン）
					gen_server:cast({global,transmission},{update_ind,dcs,[opetype,throttle,userope,speedup,torque,43000]});
				true -> pass
			end,		
			event_loop();

		{state_inf,wheelRright, [state,downslip]} ->
			case get(slipmod) of
				on ->
					gen_server:cast({global,brake}, {state_inf,dcs,[torque,calets:getSlipStopVal(wheelRright)]});
				_->false
			end,
			event_loop();
		{state_inf, cruisespeed, cspeed}->
			io:format("Cruise speed:~p~n",[cspeed]);

		{state_inf,_, _Parameter} ->
			event_loop();
		{'EXIT', Pid, Why} ->
			io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);
		Any ->
			io:format("DCS unknown type received : ~p~n", [Any]),
			event_loop()
	end.

%%get parameter from message------
getParam(Key,[Hk,Hv|_]) when Key =:= Hk->
	Hv;
getParam(Key,[_,_|T])->
	getParam(Key,T);
getParam(_,[])->
	false.

