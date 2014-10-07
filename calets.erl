%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/10/25
%%% -------------------------------------------------------------------
-module(calets).

%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-compile(export_all).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(ThrottleMax, 100000).
-define(MaxRpm, 2500).
-define(TorqueMaxRpm, 1500).
-define(TickTorque, 10000).
-define(CarWeigth,1000).
-define(G,9.8).

-define(BASERealTIME,500).

%% çÏã∆é¸ä˙ ìÆÇ´ÇääÇÁÇ©Ç…ÇµÇΩÇ¢Ç∆Ç´ÇÕílÇè¨Ç≥Ç≠Ç∑ÇÈÅB
%% É}ÉVÉìÉXÉyÉbÉNÇ™à´Ç¢Ç∆Ç´ÇÕílÇëÂÇ´Ç≠Ç∑ÇÈÅB
-define(SAVTIME,500).
-define(RealTIME,500).
%% ëOï˚é‘é‘ä‘èâä˙íl
-define(DefDistance,30).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%%  functions
%% ====================================================================

%%-------------------
%%cal distance
%%-------------------
%%Ret M
getDefDistance()->
	?DefDistance.

%% ret M
calDistance(Offset,Newf,Newmy) ->
	 Offset - (Newmy - Newf)*1000 / 3600 * ?RealTIME/?SAVTIME * ?SAVTIME/1000.

%% ret M/
calStopDistance(Rpm) ->
	[Tyre] = sim_db:getDbValue(caldata,tyre),%É^ÉCÉÑäOé¸
	 Rpm * Tyre/ 60 * ?RealTIME/1000 .

%%-------------------
%%cal NowTorque
%%-------------------

%ÉGÉìÉuÉå
calNowTorque(speedup,Nowtorqu,Remaintorqu,_WheelPosition) when Remaintorqu < 0  ->
	AddToruque = round(calets:addTorque(torqueToRpm(Nowtorqu)) div 3 * ?RealTIME/?BASERealTIME),

	if
		Nowtorqu  <  AddToruque ->
			0;
		abs(Remaintorqu)  <  AddToruque ->
			Nowtorqu + Remaintorqu; 
		true ->
			Nowtorqu  -  AddToruque
	end;
% go
calNowTorque(speedup,Nowtorqu,Remaintorqu,_WheelPosition)  when Remaintorqu =:=0 ->
	Nowtorqu;

calNowTorque(speedup,Nowtorqu,Remaintorqu,_WheelPosition)   ->
	AddToruque = round(calets:addTorque(torqueToRpm(Nowtorqu)) * ?RealTIME/?BASERealTIME),
	if
		Remaintorqu - AddToruque >= 0 ->
			Nowtorqu +  AddToruque ;
		true -> 
			Nowtorqu + Remaintorqu
	end;

%%go
calNowTorque(slipspeedup,Nowtorqu,Remaintorqu,_WheelPosition) ->
	AddToruque = round(calets:addTorque(torqueToRpm(Nowtorqu)) * ?RealTIME/?BASERealTIME),
	if
		AddToruque div 2 =:=0 ->
			Nowtorqu +  AddToruque ;
		Remaintorqu - (AddToruque div 2)  >= 0 ->
			Nowtorqu +  (AddToruque div 2) ;
		true -> 
			Nowtorqu + Remaintorqu
	end;
%%brake
calNowTorque(speeddown,Nowtorqu,Remaintorqu,_WheelPosition)  ->
	if
		Nowtorqu > abs(Remaintorqu) ->
			round( Nowtorqu + Remaintorqu * ?RealTIME/?BASERealTIME / 1.5 ) ;
		true->
			0
	end;
calNowTorque(slipspeeddown,Nowtorqu,_Remaintorqu,WheelPosition)  ->
	case  round(getDynFrictional(WheelPosition) /3 * ?RealTIME/?BASERealTIME) of
		Tick when Nowtorqu > Tick ->
			Nowtorqu - Tick ;
		_ ->
			0
	end.


%%-------------------
%%cal Remaintorqu
%%-------------------
%%ÉGÉìÉuÉå
calRemaintorqu(speedup,Nowtorqu,Remaintorqu,WheelPosition) when Remaintorqu < 0 ->
	case calNowTorque(speedup,Nowtorqu,Remaintorqu,WheelPosition) of
		NewTorue when (Nowtorqu - NewTorue ) > abs(Remaintorqu)  ->
			0;
		NewTorue ->
			Remaintorqu + (Nowtorqu -NewTorue )
	end;
%%go
calRemaintorqu(speedup,_Nowtorqu,Remaintorqu,_WheelPosition) when Remaintorqu =:=0 ->
	0;
calRemaintorqu(speedup,Nowtorqu,Remaintorqu,WheelPosition)->
	case calNowTorque(speedup,Nowtorqu,Remaintorqu,WheelPosition) of
		NewTorue when (NewTorue -Nowtorqu) < Remaintorqu  ->
			Remaintorqu - (NewTorue -Nowtorqu);
		_ -> 
			0
	end;
%%go
calRemaintorqu(slipspeedup,Nowtorqu,Remaintorqu,WheelPosition)->
	case calNowTorque(slipspeedup,Nowtorqu,Remaintorqu,WheelPosition) of
		NewTorue when (NewTorue -Nowtorqu) < Remaintorqu  ->
			Remaintorqu - (NewTorue -Nowtorqu);
		_ -> 
			0
	end;
%%brake
calRemaintorqu(speeddown,_Nowtorqu,Remaintorqu,_WheelPosition) -> 
	Remaintorqu;
%brake
calRemaintorqu(slipspeeddown,_Nowtorqu,Remaintorqu,_WheelPosition)->  
	Remaintorqu.

%%-------------------
%% ÉXÉäÉbÉvéûÇÃï\é¶ópRPM
%%-------------------
getSlipDispRpm(slipspeedup,Nowtorqu,Remaintorqu) ->
	torqueToRpm(Remaintorqu  + Nowtorqu);
getSlipDispRpm(slipspeeddown,_,_)->
	0.

%%-------------------
%% ãÛãCíÔçRÇçló∂ÇµÇΩé¿Rpm
%%-------------------
realtorqueToRpm(Toru,_Speed) when Toru =< 0->
	0;
realtorqueToRpm(Toru,Speed) ->
	torqueToRpm((Toru - calAirResistance(Speed)) ).

%%-------------------
%%ÉgÉãÉN valueme RPMïœä∑ for wheel
%%-------------------
torqueToRpm(Toru) when Toru =< 0->
	0;
torqueToRpm(Toru) ->
	round((Toru) / (?ThrottleMax / ?MaxRpm)).

%%-------------------
%% rpm ÉgÉãÉN ïœä∑
%%-------------------
rpmtoTorque(Rpm) when Rpm =< 0->
	0;
rpmtoTorque(Rpm)->
	round(Rpm * (?ThrottleMax / ?MaxRpm )).


%%-------------------
%%ÉgÉãÉNâ¡éZó 
%% ret torque
%%------------------
addTorque(Rpm) when Rpm =:= 0 ->	
	addTorque(100);
addTorque(Rpm) when Rpm <0 ->	
	0;
addTorque(Rpm)  ->	
	round(?TickTorque*(1- propotion(Rpm) * propotion(Rpm))) .

%%-------------------
%%ç≈ëÂRPMÇ∆ÇÃî‰
%% ret %
%%------------------
propotion(Rpm) when Rpm > ?MaxRpm ->
	1;
propotion(Rpm) when Rpm =< ?TorqueMaxRpm ->
	1- Rpm / ?TorqueMaxRpm;
propotion(Rpm) when Rpm > ?TorqueMaxRpm ->
	1- ( ?TorqueMaxRpm *2 - Rpm )/ ?TorqueMaxRpm.

%%-------------------
%%êÇíºçRóÕ
%% ret N
%%------------------
calForce()->
	?CarWeigth * ?G.

%%-------------------
%%ñÄéCóÕåvéZ
%% ret N
%%------------------
calFrictional(sta_fine)->
	calForce()*0.7;
calFrictional(sta_rain)->
	calForce()*0.5;
calFrictional(sta_snow)->
	calForce()*0.15;
calFrictional(dyn_fine)->
	calForce()*0.5;
calFrictional(dyn_rain)->
	calForce()*0.2;
calFrictional(_)->
	calForce()*0.05.

%%-------------------
%% ìÆñÄéCóÕéÊìæ
%% ret N
%%------------------
getDynFrictional(WheelPosition)->
	Dyn = sim_db:getDbValue(loadenvs,WheelPosition,dynamictor),
	round(Dyn *calForce()).

%%-------------------
%% ê√é~ñÄéCóÕéÊìæ
%% ret N
%%------------------
getStaFrictional(WheelPosition)->
	Str = sim_db:getDbValue(loadenvs,WheelPosition,statictor),
	round(Str *calForce() ).

%%-------------------
%%ÉXÉäÉbÉväJénÅ@îªíË
%% ret mtrue ptrue false
%%-------------------
isSlipStart(Remaintorqu,WheelPosition)->
	isSlip(Remaintorqu,getSlipStartVal(WheelPosition)).


getSlipStartVal(WheelPosition)->
	getStaFrictional(WheelPosition)*2.

getSlipStopVal(WheelPosition)->
	getDynFrictional(WheelPosition)*2.

%%-------------------
%%ÉXÉäÉbÉví‚é~Å@îªíË
%% ret true false
%%-------------------
isSlipEnd(Torqu,WheelPosition) ->
	case isSlip(Torqu,getSlipStopVal(WheelPosition)) of
		false ->
			true;
		_->
			false
	end.


%%-------------------
%%ÉXÉäÉbÉv îªíË
%% ret mtrue ptrue false
%%-------------------
isSlip(Torqu,Frictional)  when Frictional < abs(Torqu)->
	if
		Torqu < 0 ->
				mtrue;
		true -> 
			ptrue
	end;
isSlip(_,_)->
	false.


%%-------------------
%%ãÛãCíÔçR
%% ÇeÅÅÇo(1)ÅñÇb(0.35)ÅñÇrÅñÇu(speed)^ÇQÅ^ÇQ  
%% ret N
%%-------------------
calAirResistance(Speed)->
	round(0.35*getAirResistanceArea()*getSpeed(Speed)*getSpeed(Speed)/2).

%%-------------------
%%ãÛãCíÔçRñ êœ
%% ret m2
%%-------------------
getAirResistanceArea()->
	[Ret] = sim_db:getDbValue(caldata,area),
	Ret.
%%-------------------
%%ãÛãCíÔçRspeed
%% ret m/sec
%%-------------------
getSpeed(Speed)->

	try Speed*1000/3600 of
		Ret -> Ret
	catch	
		_ ->io:format("getSpeed Error: ~p ~n", [Speed])
	end.



%%-------------------
%%åvéZíPà éûä‘
%%  ms
%%-------------------
getSavTime()->
	?SAVTIME.
getRealTime()->
	?RealTIME.

