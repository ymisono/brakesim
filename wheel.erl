%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% Ver.2.0 : 2010/10/28
%%% -------------------------------------------------------------------
-module(wheel).

%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/1,dic_put/2]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(RANGE, 100000).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start(WheelPosition) ->
%%	io:format("-------------wheel start node=~p--------------~n",[ erlang:node()]),
	put(wheelPosition, WheelPosition),
	SelfPid = self(),
	process_flag(trap_exit, true),
	Pid =  spawn_link(fun() -> wheel_gui:start(SelfPid, WheelPosition) end),
	{ok,TRef} = timer:send_interval(calets:getRealTime(),{timeout}),
	put(cresc,TRef),
	put(child_gui, Pid),
	put(state,0),
	put(nowtorqu,0),
	put(remaintorqu,0),
	put(rpm,0),
	put(speed,0),
	event_loop().


%% ====================================================================
%% Internal functions
%% ====================================================================

event_loop() ->
	receive
		{state_inf, brake, Parameter} ->
			state(brake,Parameter),
			event_loop();

		{state_inf, transmission, [torqu,Torqu]} ->%%wheelRear îÒãÏìÆó÷
			put(rpm,calets:realtorqueToRpm(Torqu,get(speed))),
			put(nowtorqu,Torqu),
			event_loop();%%disp rpm when timeout

		{state_inf, transmission, Parameter} ->
			state(throttle,Parameter),
			event_loop();


		{state_inf, output, Parameter} ->
			put(speed,getParam(speed,Parameter)),
			event_loop();

		{update_ind, _From, Parameter} ->
			state(update,Parameter),
			event_loop();

		{'EXIT', Pid, Why} ->
			io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);
		{timeout} ->
			state(timeout,[]),
			event_loop();

		Any ->
			io:format("wheel unknown type received : ~p~n", [Any]),
			event_loop()


	end.

%%-------------------------------------
%% tool
%%-------------------------------------
%%TIMER
%timer(PPid)->
%	receive 
%		cancel ->
%			void
%	after ?TIMTICK ->
%		PPid ! {timeout},
%		timer(PPid)
%	end.
%
%%get parameter from message------
getParam(Key,[Hk,Hv|_]) when Key =:= Hk->
	Hv;
getParam(Key,[_,_|T])->
	getParam(Key,T);
getParam(_,[])->
	false.


dic_put(nowtorqu,Val) when Val =< ?RANGE,Val >= 0  ->
	put(nowtorqu,Val);
dic_put(nowtorqu,_)  ->
	false;
dic_put(remaintorqu,Val) when Val =< ?RANGE,Val >= -?RANGE  ->
	case get(nowtorqu) of
		Now when Now =:= 0 ->
			put(remaintorqu,Val);
		Now when (Now + Val) =< ?RANGE  ->
			put(remaintorqu,Val);
		_ -> 	io:format("wheel remaintorqu_put Error2 Now= ~p,Val=~p ~n", [get(nowtorqu),Val]),
			false
	end;
dic_put(remaintorqu,Val)  ->
	io:format("wheel remaintorqu_put Error Now= ~p,Val=~p ~n", [get(nowtorqu),Val]),
	false;
dic_put(rpm,Val) ->
	put(rpm,Val);
dic_put(Key,Val)->
	io:format("wheel dic_put Error Key=~p,Val=~p ~n", [Key,Val]).


%%-------------------------------------
%% state 
%%-------------------------------------
state(Msg,Parameter)->
	Ret = state(get(state),Msg,
		fun(Paraname)->getParam(Paraname,Parameter) end,
		fun(Req,P2)->
			 case Req of 
				get -> {Dicparaname}= P2,
					get(Dicparaname);
				put -> {Dicparaname,Val}= P2,
				%	dic_put(Dicparaname,Val)
					put(Dicparaname,Val)
		end 
		end),
%%%print
	case get(wheelPosition) of
		nwheelFright when Ret >0  ->
			io:format("wheel state msg=~p nowtorqu=~p,remaintorqu=~p,rpm=~p,next state=~p ~n", [Msg, get(nowtorqu),get(remaintorqu),get(rpm),Ret]),
			io:format("wheel state Para=~p  ~n", [Parameter]);
		_ -> false
	end,

	put(state,Ret).


%%-------------------------------------
%% state tansmition
%%------------------------------------
%% stn=0 ==================
state(0,brake,MsgPara,DicPara) ->
	case DicPara(get,{rpm}) of
		Rpm when Rpm > 0 ->
			case state_parts_sav(MsgPara,DicPara)	of
				true -> 2;
				upslip -> 2;
				downslip -> 
					gen_server:cast({global,dcs}, {state_inf,DicPara(get,{wheelPosition}),[state,downslip]}),
					4;
				false-> 0
			end;
		_->
			0
	end;

state(0,throttle,MsgPara,DicPara) ->
	Rpm = DicPara(get,{rpm}),
	case MsgPara(userope) of
		speeddown when Rpm =< 0 -> 
			0;
		_ ->
			case state_parts_sav(MsgPara,DicPara)	of
				true -> 1;
				upslip -> 3;
				downslip -> 1;
				false-> 0
			end
	end;
state(0,update,MsgPara,DicPara) ->
	case state_parts_update(MsgPara,DicPara)	of
		brakenormal -> 2;
		brakeslip  -> 4;
		throttlenormal -> 1;
		throttleslip  -> 3;
		false-> 1
	end;

state(0,timeout,_,DicPara) ->
	DicPara(get,{child_gui}) !{state_inf,parwheel,DicPara(get,{rpm})},
	0;

%% stn =1 ========speed up============

state(1,update,MsgPara,DicPara) ->
	case state_parts_update(MsgPara,DicPara)	of
		brakenormal -> 2;
		brakeslip  -> 4;
		throttlenormal -> 1;
		throttleslip  -> 3;
		false-> 1
	end;

state(1,brake,MsgPara,DicPara) ->
	DicPara(put,{remaintorqu,0}),%%throttle deactive
	case state_parts_sav(MsgPara,DicPara)	of
		true -> 2;
		upslip -> 2;
		downslip -> 
			gen_server:cast({global,dcs}, {state_inf,DicPara(get,{wheelPosition}),[state,downslip]}),
			4;
		false-> 0
	end;

state(1,throttle,MsgPara,DicPara) ->
	case state_parts_sav(MsgPara,DicPara )	of
		true -> 1;
		upslip -> 3;
		downslip -> 1;
		false-> 0
	end;

state(1,timeout,MsgPara,DicPara) ->
	Ret = state_parts(speedup,MsgPara,DicPara),
	Ret;

%% stn =2 ========speed down============
state(2,update,MsgPara,DicPara) ->
	case state_parts_update(MsgPara,DicPara) of
		brakenormal -> 2;
		brakeslip  -> 4;
		throttlenormal -> 1;
		throttleslip  -> 3;
		false-> 2
	end;

state(2,brake,MsgPara,DicPara) ->
	case state_parts_sav(MsgPara,DicPara)	of
		true -> 2;
		upslip -> 2;
		downslip -> 
			gen_server:cast({global,dcs}, {state_inf,DicPara(get,{wheelPosition}),[state,downslip]}),
			4;
		false-> 0
	end;


state(2,throttle,_MsgPara,DicPara) ->
	gen_server:cast({global,throttle}, {state_inf,DicPara(get,{wheelPosition}),[torque,DicPara(get,{nowtorqu})]}),
	2;

state(2,timeout,MsgPara,DicPara) ->
	case DicPara(get,{wheelPosition}) of
		wwheelFright->
			io:format("stopping ~p [M] ~n", [calets:calStopDistance(DicPara(get,{rpm}))]);
		_->false
	end,
	case state_parts(speeddown,MsgPara,DicPara) of
		0-> 
			case DicPara(get,{wheelPosition}) of
				wheelFright->
					gen_server:cast({global,throttle}, {state_inf,wheelFright,[torque,0]});
				_ ->
					false
			end,
				
			case DicPara(get,{nowtorqu}) of
				Now when Now >0 ->
					DicPara(put,{remaintorqu,0 - Now}),
					1;
				_->
					0
			end;
		Ret ->
			 Ret
	end;

%% stn =3 ========slip speed up ============
state(3,update,MsgPara,DicPara) ->
	case state_parts_update(MsgPara,DicPara) of
		brakenormal -> 
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstop},
			2;
		brakeslip  ->
			 4;
		throttlenormal ->
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstop},
			1;
		throttleslip  ->
			3;
		false-> 3
	end;

state(3,brake,MsgPara,DicPara) ->
	case state_parts_sav(MsgPara,DicPara)	of
		downslip -> 
			gen_server:cast({global,dcs}, {state_inf,DicPara(get,{wheelPosition}),[state,downslip]}),
			4;
		_-> 
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstop},
			2
	end;

state(3,throttle,MsgPara,DicPara) ->
	state_parts_sav(MsgPara,DicPara),
	3;

state(3,timeout,MsgPara,DicPara) ->
	Ret = state_parts(slipspeedup,MsgPara,DicPara),
	Ret;

%% stn =4 ========slip speed down ============
state(4,update,MsgPara,DicPara) ->
	case state_parts_update(MsgPara,DicPara) of
		brakenormal -> 
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstop},
			2;
		brakeslip  -> 
			4;
		throttlenormal -> 
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstop},
			1;
		throttleslip  ->
			3;
		false-> 
			4
	end;

state(4,brake,MsgPara,DicPara) ->
	state_parts_sav(MsgPara,DicPara),
	 4;

state(4,throttle,_MsgPara,DicPara) ->
	gen_server:cast({global,throttle}, {state_inf,DicPara(get,{wheelPosition}),[torque,DicPara(get,{nowtorqu})]}),
	4;

state(4,timeout,MsgPara,DicPara) ->
	case state_parts(slipspeeddown,MsgPara,DicPara) of
		0-> 0;
		Ret -> Ret
	end;

%% other

state(Stn,Msg,_MsgPara,_DicPara) ->
	state_parts(Stn,Msg,0),
	Stn.

%%-------------------------------------
%% state tansmition parts 
%%------------------------------------
state_parts_sav(MsgPara,DicPara) ->

	Tor = MsgPara(torque),
	Now = DicPara(get,{nowtorqu}),
	case MsgPara(userope) of
		speedup ->
			case MsgPara(opetype) of
				throttle ->
					DicPara(put,{remaintorqu,Tor - Now});
				_->
					DicPara(put,{remaintorqu,0 - Tor})
			end,
			case calets:isSlipStart(DicPara(get,{remaintorqu}) div 4,DicPara(get,{wheelPosition})) of
				ptrue -> upslip;
				mtrue -> true;
				false -> true
			end; 
		speeddown -> 
			case MsgPara(opetype) of
				throttle when Now < Tor->
					DicPara(put,{remaintorqu,Tor - Now});
				throttle ->
					DicPara(put,{remaintorqu,Tor - Now});
				_->
					DicPara(put,{remaintorqu,0 - Tor})
			end,
			case calets:isSlipStart(DicPara(get,{remaintorqu}),DicPara(get,{wheelPosition}))  of
				mtrue -> downslip;
				ptrue -> true;
				false -> true
			end ;

		_ -> false
	end.



state_parts_update(MsgPara,DicPara) ->
	case MsgPara(opetype) of
		brake ->
			case state_parts_sav(MsgPara,DicPara)	of
				true -> brakenormal;
				upslip -> brakeslip;
				downslip -> brakenormal;
				false-> false
			end;

		throttle ->
			case state_parts_sav(MsgPara,DicPara)	of
				true -> throttlenormal;
				upslip -> throttleslip;
				downslip -> throttlenormal;
				false-> false
			end;
		Any -> io:format("wheel state_parts_update opetype NG : ~p ~n", [Any]),
			false
	end.


%%
%%@return Stn
%%
%%---------------------------
%%speeddown 
%%---------------------------
state_parts(speeddown,_MsgPara,DicPara) ->

	Remaintorqu = calets:calRemaintorqu(speeddown,DicPara(get,{nowtorqu}),DicPara(get,{remaintorqu}),DicPara(get,{wheelPosition})),
	NowToruq =   calets:calNowTorque(speeddown,DicPara(get,{nowtorqu}),DicPara(get,{remaintorqu}),DicPara(get,{wheelPosition})),
	modify_dicpara(DicPara,Remaintorqu,NowToruq),
	if 
		Remaintorqu =:= 0-> 0;
		true -> 2
	end;

	
%%---------------------------
%%speedup 
%%---------------------------
state_parts(speedup,_MsgPara,DicPara)  ->

	Remaintorqu = calets:calRemaintorqu(speedup,DicPara(get,{nowtorqu}),DicPara(get,{remaintorqu}),DicPara(get,{wheelPosition})),
	NowToruq =   calets:calNowTorque(speedup,DicPara(get,{nowtorqu}),DicPara(get,{remaintorqu}),DicPara(get,{wheelPosition})),
	modify_dicpara(DicPara,Remaintorqu,NowToruq),

	case Remaintorqu =/=0 of
		true   ->
			1;
		_ -> 
			0
	end;

%%---------------------------
%%splip down
%%---------------------------
state_parts(slipspeeddown,_MsgPara,DicPara) ->
	
	DispRpm = 0,
	Remaintorqu = calets:calRemaintorqu(slipspeeddown,DicPara(get,{nowtorqu}),DicPara(get,{remaintorqu}),DicPara(get,{wheelPosition})),
	NowToruq =   calets:calNowTorque(slipspeeddown,DicPara(get,{nowtorqu}),DicPara(get,{remaintorqu}),DicPara(get,{wheelPosition})),
	slip_modify_dicpara(DicPara,DispRpm,Remaintorqu,NowToruq),

	Remret =  calets:isSlipEnd(Remaintorqu,DicPara(get,{wheelPosition})),

	case calets:isSlipEnd(NowToruq,DicPara(get,{wheelPosition})) of
		true ->
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstop},
			 2;
		false when Remret =:= true-> 
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstop},
			 2;
		false -> 
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstart},
			4
	end;
%%---------------------------
%%slipup	
%%---------------------------
state_parts(slipspeedup,_MsgPara,DicPara)  ->

	DispRpm = calets:getSlipDispRpm(slipspeedup,DicPara(get,{nowtorqu}),DicPara(get,{remaintorqu})),
	Remaintorqu = calets:calRemaintorqu(slipspeedup,DicPara(get,{nowtorqu}),DicPara(get,{remaintorqu}),DicPara(get,{wheelPosition})),
	NowToruq =  calets:calNowTorque(slipspeedup,DicPara(get,{nowtorqu}),DicPara(get,{remaintorqu}),DicPara(get,{wheelPosition})),
	slip_modify_dicpara(DicPara,DispRpm,Remaintorqu,NowToruq),

	case calets:isSlipEnd(Remaintorqu,DicPara(get,{wheelPosition})) of
		false  -> 
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstart},
			3;
		true -> 
			DicPara(get,{child_gui}) !{state_inf,parwheel,slipstop},
			1
	end;


%	io:format("wheel Remaintorqu : ~p ~n", [Remaintorqu]),

%%---------------------------
%% other = error	
%%---------------------------
state_parts(Stn,Msg,_)  ->
	io:format("wheel state_parts unknown type received : ~p : ~p~n", [Stn,Msg]),
	false.

%%
%%---------------------------
%%modify intra data	for normal for ãÏìÆó÷
%%---------------------------
modify_dicpara(DicPara,Remaintoruq,NowToruq)->
	Rpm =  calets:realtorqueToRpm(NowToruq,DicPara(get,{speed})),

	DicPara(put,{rpm,Rpm}),
	DicPara(put,{remaintorqu,Remaintoruq}),
	DicPara(put,{nowtorqu,NowToruq}),
	%%send message
	gen_server:cast({global,dcs}, {state_inf,DicPara(get,{wheelPosition}),[rpm,Rpm,remaintorqu,Remaintoruq]}),

	gen_server:cast({global,output}, {state_inf,DicPara(get,{wheelPosition}),[remaintorqu,Remaintoruq]}),

	gen_server:cast({global,transmission}, {state_inf,DicPara(get,{wheelPosition}),[torqu,NowToruq,remaintorqu,Remaintoruq]}),
	DicPara(get,{child_gui}) !{state_inf,parwheel,Rpm}.

%%---------------------------
%%modify intra data	for slipping for ãÏìÆó÷
%%---------------------------
slip_modify_dicpara(DicPara,DispRpm,Remaintoruq,NowToruq)->
	Rpm =  calets:realtorqueToRpm(NowToruq,DicPara(get,{speed})),

	DicPara(put,{rpm,Rpm}),
	DicPara(put,{remaintorqu,Remaintoruq}),
	DicPara(put,{nowtorqu,NowToruq}),
	%%send message
	gen_server:cast({global,dcs}, {state_inf,DicPara(get,{wheelPosition}),[rpm,DispRpm,remaintorqu,Remaintoruq]}),

	gen_server:cast({global,output}, {state_inf,DicPara(get,{wheelPosition}),[remaintorqu,Remaintoruq]}),

	gen_server:cast({global,transmission}, {state_inf,DicPara(get,{wheelPosition}),[torqu,NowToruq,remaintorqu,Remaintoruq]}),
	DicPara(get,{child_gui}) !{state_inf,parwheel,DispRpm}.

	

%%io:format("wheel state_inf received in ~p  Para=~p ~n", [get(wheelPosition), Parameter]),



