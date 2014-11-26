%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(distance).


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/1]).
%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start(SidePosition) ->
	put(sidePosition, SidePosition),    
	SelfPid = self(),
	process_flag(trap_exit, true),
	Pid =  spawn_link(fun() -> distance_gui:start(SelfPid, SidePosition) end),
	put(child_gui, Pid),
	
	if 
		SidePosition =:= frsensor -> 
			{ok,TRef} = timer:send_interval(calets:getSavTime(),{timeout, timer,[]}),
			put(timer,TRef);
		true ->
			false
	end,

	event_loop(idle),
	ok.


%% ====================================================================
%% Internal functions
%% ====================================================================
event_loop(State) ->
	receive
		{state_inf,chgui,Parameters} ->
			gen_server:cast({global,dcs},{state_inf, get(sidePosition),[distance,Parameters]}),
			%% save
			gen_server:cast({global,sim_db},{msgsav,get(sidePosition),{state_inf, chgui,Parameters}}),
			event_loop(State);

		{scenario_req, scenario,[scenario,Scr]} ->
			put(distance,calets:getDefDistance()),
			put(scenario,Scr),
			get(child_gui)!{scenario_req,pare,[]},
			event_loop(State);

		{scenario_can, scenario,_} ->
			put(scenario,[]),
			get(child_gui)!{scenario_res,pare,[]},
			event_loop(State);

		{logstart_ind, scenario,_} ->
			event_loop(logging);

		{logstop_ind, scenario,_} ->
			event_loop(idle);
			
		{timeout,timer,_}->
			gen_server:cast({global,output},{getinfo_req, get(sidePosition),[speed,0]}),
			event_loop(State);

		{getinfo_res,output,Parameters} ->
			Speed = getParam(speed,Parameters),
			%%scenario process
			case get(scenario) of
				[[T]|[]] ->	%%scenario finish
					gen_server:cast({global,scenario},{scenario_res,get(sidePosition),[]}),
					get(child_gui)!{scenario_res,pare,[]},
					NewDis = calets:calDistance(get(distance),T,Speed),
					put(distance,NewDis),
					put(scenario,[]);
				[[T]|B] -> 
					NewDis = calets:calDistance(get(distance),T,Speed),
					get(child_gui)!{state_inf,pare,[distance,NewDis]},
					put(distance,NewDis),
					put(scenario,B);
				_->
					false
			end,
			%%log process
			if
				State =:= logging ->
					gen_server:cast({global,scenario},{log_inf,get(sidePosition),[speed,Speed,distance,get(distance)]});
				true->
					false
			end,

			event_loop(State);

		{'EXIT', Pid, Why} ->
			io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);

		Any ->
			io:format("distance unknown type received : ~p~n", [Any]),
			event_loop(State)
	end.
%io:format("scenario_req output received : ~p~n", [get(scenario)]),
	
%% ====================================================================
%% Intra functions
%% ====================================================================
%%get parameter from message
getParam(Key,[Hk,Hv|_]) when Key =:= Hk->
	Hv;
getParam(Key,[_,_|T])->
	getParam(Key,T);
getParam(_,[])->
	false.



