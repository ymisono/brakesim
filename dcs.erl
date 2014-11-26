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
			_Des = getParam(distance,Parameter),
			event_loop();
		{state_inf,bksensor, Parameter} ->
			case getParam(distance,Parameter) of
				Des when Des >0 ->
					put(slipmod,on);
				_->
					false
			end,
			event_loop();
		{state_inf,risensor, Parameter} ->
			_Des = getParam(distance,Parameter),
			event_loop();
		{state_inf,lfsensor, Parameter} ->
			_Des = getParam(distance,Parameter),
			event_loop();
		{state_inf,throttle, _Parameter} ->
			event_loop();
		{state_inf,brake, _Parameter} ->
			event_loop();

		{state_inf,wheelRright, [state,downslip]} ->
			case get(slipmod) of
				on ->
					gen_server:cast({global,brake}, {state_inf,dcs,[torque,calets:getSlipStopVal(wheelRright)]});
				_->false
			end,
			event_loop();
			
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

