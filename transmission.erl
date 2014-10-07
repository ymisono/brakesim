%%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/06/15
%%% -------------------------------------------------------------------
-module(transmission).

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
	put(now,{0,0,0,0}),
	event_loop().


%% ====================================================================
%% Internal functions
%% ====================================================================
event_loop() ->
	receive
		{state_inf, throttle, Parameter} ->
			gen_server:cast({global,wheelFright},{state_inf,transmission,Parameter}),
			gen_server:cast({global,wheelFleft},{state_inf,transmission,Parameter}),

			gen_server:cast({global,output},{state_inf,transmission,[torqu,avgTru()]}),
			gen_server:cast({global,wheelRright},{state_inf,transmission,[torqu,avgTru()]}),
			gen_server:cast({global,wheelRleft},{state_inf,transmission,[torqu,avgTru()]}),
			event_loop();

		{state_inf, wheelRright, _Parameter} ->
			event_loop();

		{state_inf, wheelRleft, _Parameter} ->
			event_loop();

		{state_inf, wheelFright, Parameter} ->
			{_Fr,Fl,Rr,Rl} = get(now),
			put(now,{getParam(torqu,Parameter),Fl,Rr,Rl}),
			event_loop();

		{state_inf, wheelFleft, Parameter} ->
			{Fr,_Fl,Rr,Rl} = get(now),
			put(now,{Fr,getParam(torqu,Parameter),Rr,Rl}),
			event_loop();


		{update_ind, _From, Parameter} ->
			gen_server:cast({global,wheelFright},{update_ind,transmission,Parameter}),
			gen_server:cast({global,wheelFleft},{update_ind,transmission,Parameter}),

			event_loop();

		{'EXIT', Pid, Why} ->
			io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);
		_Any ->
			io:format("transmission unknown type received : ~p~n", [_Any]),
			event_loop()
	after 200 ->
		gen_server:cast({global,output},{state_inf,transmission,[torqu,avgTru()]}),
		gen_server:cast({global,wheelRright},{state_inf,transmission,[torqu,avgTru()]}),
		gen_server:cast({global,wheelRleft},{state_inf,transmission,[torqu,avgTru()]}),
		event_loop()
		
	end.

%io:format("transmission print : ~p~n", [get(now)]),

%%get parameter from message------
getParam(Key,[Hk,Hv|_]) when Key =:= Hk->
	Hv;
getParam(Key,[_,_|T])->
	getParam(Key,T);
getParam(_,[])->
	false.

avgTru()->
	{Fr,Fl,_Rr,_Rl} = get(now),
	(Fr+Fl) div 2.
	



