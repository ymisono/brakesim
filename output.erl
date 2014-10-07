%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(output).

%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0,calspeedMethod/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(TIMTICK, 400).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
	SelfPid = self(),
	process_flag(trap_exit, true),
	Pid =  spawn_link(fun() -> output_gui:start(SelfPid) end),
	spawn_link(fun() -> timer(SelfPid) end),
	put(child_gui, Pid),
	put(state,0),
	put(stopc,-1),
	put(speed,0),
	put(fl,{fl,0,0}),
	put(fr,{fr,0,0}),
	put(rr,{rr,0,0}),
	put(rl,{rl,0,0}),
	event_loop(),
	ok.


%% ====================================================================
%% Internal functions
%% ====================================================================

event_loop() ->
	receive
		{state_inf, wheelFright, Parameter} ->
			put(fr,{fr,0,getParam(remaintorqu,Parameter)}),
			event_loop();
		{state_inf, wheelFleft, Parameter} ->
			put(fl,{fl,0,getParam(remaintorqu,Parameter)}),
			event_loop();
		{state_inf, wheelRright, Parameter} ->
			put(rr,{rr,0,getParam(remaintorqu,Parameter)}),
			event_loop();
		{state_inf, wheelRleft, Parameter} ->
			put(rl,{rl,0,getParam(remaintorqu,Parameter)}),
			event_loop();

		{state_inf, throttle, _Parameter} ->
			%%state(proced,Parameter),
			event_loop();
		{state_inf,brake, Parameter} ->
			state(proced,Parameter),
			event_loop();
	
		{state_inf,transmission, Parameter} ->
			state(transmission,Parameter),
			event_loop();

		{state_inf, _From, Parameter} ->
			state(none,Parameter),
			event_loop();
		{update_ind, _From, Parameter} ->
			state(none,Parameter),
			event_loop();

		{getinfo_req,From,[speed,_]}->
			gen_server:cast({global,From},{getinfo_res,output,[speed,get(speed)]}),
			event_loop();

		{'EXIT', Pid, Why} ->
			io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);

		{timeout} ->
			state(timeout,[get(fl),get(fr),get(rr),get(rl)]),
			event_loop();

		Any ->
			io:format("unknown type received : ~p~n", [Any]),
			event_loop()

	end.

%%TIMER
timer(PPid)->
	receive 
		cancel ->
			void
	after ?TIMTICK ->
		PPid ! {timeout},
		timer(PPid)
	end.


%%-------------------------
%% state tansmition
%%-------------------------
state(Msg,Parameter)->
	State = get(state),
	Ret = state(State,Msg,Parameter),
	put(state,Ret).

%%get parameter from message
getParam(Key,[Hk,Hv|_]) when Key =:= Hk->
	Hv;
getParam(Key,[_,_|T])->
	getParam(Key,T);
getParam(_,[])->
	false.

	

%%------------------------
state(Stn,transmission,Parameter) when Stn=:=0 ->
	Speed = calspeedMethod(getParam(torqu,Parameter)),
	put(speed,Speed),
	gen_server:cast({global,wheelFright},{state_inf,output,[speed,Speed]}),
	gen_server:cast({global,wheelFleft },{state_inf,output,[speed,Speed]}),
	gen_server:cast({global,wheelRright},{state_inf,output,[speed,Speed]}),
	gen_server:cast({global,wheelRleft },{state_inf,output,[speed,Speed]}),
	0;

state(Stn,proced,Parameter) when Stn=:=0 ->
	Ret = state_parts(statechg,Parameter),
	if
		Ret =:= 2 ->
			put(stopc,0);
		true->false
	end,
	0;


state(Stn,timeout,Parameter) when Stn=:=0 ->
	put(stopc,countTime(get(stopc))),
%%			io:format("output state  : ~p,~n", [get(stopc)]),
%%		io:format("output state  : ~p,~n", [Parameter]),

	
	CalBalIn = lists:map(fun({_Pos,Rpm,Rtoru})-> Rpm+Rtoru end,Parameter),
	Balance = calbalance(CalBalIn),

	state_parts_speedchg(get(stopc),get(speed),Balance),
	0;

%%other
state(Stn,_Msg,Parameter) ->
	state_parts(none,Parameter),
	Stn.

%%-------------------------
%% state tansmition parts
%%-------------------------
%%rpm(m/m) -> Km/h
calspeedMethod(Toru)->
	Rpm =  calets:realtorqueToRpm(Toru,get(speed)),
	[Tyre] = sim_db:getDbValue(caldata,tyre),%É^ÉCÉÑäOé¸
	round(Rpm * 60 *Tyre /1000).


countTime(In) when In < 0 ->
	-1;
countTime(In) ->
	In +1.

%%
calbalance([0,0,0,0])->
	[25,25,25,25];
calbalance(L1)->
	Sum = lists:sum(L1),
	lists:map(fun(In)-> In * 100 div Sum end,L1).

%%èÛë‘ïœçX
state_parts(Name,Parameter) when Name =:= statechg->
	Ret = getParam(userope,Parameter),
	Tor = getParam(torque,Parameter),
	case  Ret of 
		speedup when Tor > 0  -> 1;
		speeddown when Tor > 0 -> 2;
		_->	0
	end;

state_parts(_Name,_Parameter)  ->false.


state_parts_speedchg(Stopc,Speed,Blance)->
	get(child_gui) !{state_inf,paroutput,speed,Speed},
%        speedMeter:sendmsg(Speed),

	get(child_gui) !{state_inf,paroutput,balance,Blance},
	if 
		Speed =:= 0,Stopc > 0 ->
			get(child_gui) !{state_inf,paroutput,stopt,?TIMTICK * get(stopc)},
			put(stopc,-1);
		true ->
			false
	end.
	
