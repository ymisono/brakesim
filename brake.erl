%%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(brake).

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
	SelfPid = self(),
	process_flag(trap_exit, true),
	Pid =  spawn_link(fun() -> brake_gui:start(SelfPid) end),
	put(val,0),
	put(child_gui, Pid),
	event_loop().


%% ====================================================================
%% Internal functions
%% ====================================================================
event_loop() ->
	receive
		{state_inf, chgui,Parameters} ->
			Old = get(val),
			put(val,Parameters),
%io:format("brake proces ~p,~p~n", [Old, Parameters]),
			CrePara = {Old,Parameters},
			gen_server:cast({global,wheelFright},{state_inf, brake,crePara(CrePara)}),
			gen_server:cast({global,wheelFleft},{state_inf, brake,crePara(CrePara)}),
			gen_server:cast({global,wheelRright},{state_inf, brake,crePara(CrePara)}),
			gen_server:cast({global,wheelRleft},{state_inf, brake,crePara(CrePara)}),
			gen_server:cast({global,dcs},{state_inf, brake,crePara(CrePara)}),
			gen_server:cast({global,output},{state_inf, brake,crePara(CrePara)}),
			%% save
			gen_server:cast({global,sim_db},{msgsav,brake,{state_inf, chgui,Parameters}}),

			event_loop();

		{state_inf,dcs,[torque,Toru]} ->
			get(child_gui)! {state_inf,parent,Toru},
			event_loop();

		{'EXIT', Pid, Why} ->
			io:format("brake child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);
		_Any ->
%%			io:format("unknown type received : ~p~n", [_Any])
			event_loop()
	end.

crePara({Old,Torque})->
	if

		Old =< Torque ->[opetype,brake,userope,speeddown,torque,Torque ];
		true->		[opetype,brake,userope,speedup,torque,Torque ]
	end.
