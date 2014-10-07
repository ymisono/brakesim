%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/24
%%% -------------------------------------------------------------------
-module(sim_db).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-import(lists, [foreach/2]).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-compile(export_all).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SAVTIME,100).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(loadenvs, {wposition,statictor, dynamictor, dummy}).
-record(maxNo, {name, number}).
-record(scenario, {scname, scenario}).
-record(caldata, {key, val}).%0603


%% ====================================================================
%% External functions
%% ====================================================================

init_mnesia(Node) ->
	mnesia:create_schema(Node),
	lists:foreach(
		fun(X)->
			ok = rpc:call(X,mnesia,start,[])
		end,
		Node),
	mnesia:create_table(loadenvs, [{disc_copies,Node},{attributes, record_info(fields, loadenvs)}]),	
	mnesia:create_table(maxNo,   [{disc_copies,Node},{attributes, record_info(fields, maxNo)}]),
	mnesia:create_table(scenario, [{disc_copies,Node},{attributes, record_info(fields, scenario)}]),
	mnesia:create_table(caldata, [{disc_copies,Node},{attributes, record_info(fields, caldata)}]),%0603
	add_Maxno_item(0),
	add_Caldat(tyre,2),%0603
	add_Caldat(area,1.9),%v2
	lists:foreach(
		fun(X)->
			rpc:call(X,mnesia,stop,[])
		end,
		Node).

reset_tables() ->
	mnesia:clear_table(loadenvs),
	mnesia:clear_table(maxNo),
	mnesia:clear_table(scenario),
	mnesia:clear_table(caldata),%0603
	add_Maxno_item(0),
	add_Caldat(tyre,2),%0603
	add_Caldat(area,1.9),%v2
%--------patch---------
	sim_db:add_loavenv_item(wheelFright,0.7,loadenv:calDynFrictional(0.7),1000),
	sim_db:add_loavenv_item(wheelFleft,0.7,loadenv:calDynFrictional(0.7),1000),
	sim_db:add_loavenv_item(wheelRright,0.7,loadenv:calDynFrictional(0.7),1000),
	sim_db:add_loavenv_item(wheelRleft,0.7,loadenv:calDynFrictional(0.7),1000).
%-----------------

unittestBg() ->
	spawn(fun()->start([node1@localhost,node2@localhost])end).


start(Node) ->
	%%SelfPid = self(),
	%%process_flag(trap_exit, true),
	%%Pid =  spawn_link(fun() -> sim_db:start(SelfPid) end),
	%%put(child_gui, Pid),
	lists:foreach(
		fun(X)->
			ok = rpc:call(X,mnesia,start,[])
		end,
		Node),
 	mnesia:wait_for_tables([loadenvs,scenario,maxNo], 20000),
	spawn_link(fun() -> timer(self()) end),
	event_loop(off,[]).


%%--------------
%%DB method
%%---------------

getDbValue(loadenvs) ->
    do(qlc:q([X	|| X <- mnesia:table(loadenvs)])).

%0603
getDbValue(caldata,Datname) ->
    do(qlc:q([X#caldata.val || X <- mnesia:table(caldata),
			     X#caldata.key =:= Datname]));
getDbValue(loadenvs,Key) ->
    do(qlc:q([{X#loadenvs.statictor,X#loadenvs.dynamictor,X#loadenvs.dummy} 
			|| X <- mnesia:table(loadenvs),
			X#loadenvs.wposition =:= Key]));


getDbValue(scenario,Scname) ->
    do(qlc:q([X#scenario.scenario || X <- mnesia:table(scenario),
			     X#scenario.scname =:= Scname])).

getDbValue(loadenvs,Key,Member) ->
	case getDbValue(loadenvs,Key) of
		[{Statictor,Dynamictor,Resisttor}] -> 
			case Member of
				statictor -> Statictor;
				dynamictor -> Dynamictor;
				resisttor->Resisttor;
				_->	io:format("sim_db getDbValue/3 Member error Member=~p ~n", [Member]),
					false
			end;

		Any ->	io:format("sim_db getDbValue/3 getvalue error Ret=~p ~n", [Any])
	end.

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.



add_loavenv_item(Wposition,Statictor, Dynamictor, Resisttor) ->
    Row = #loadenvs{wposition=Wposition,statictor=Statictor, dynamictor=Dynamictor, dummy=Resisttor},
    F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).

%v2
update_areRegist (Area)->
	gen_server:cast({global,loadenv},{state_inf,sim_db,[area,Area]}),
	add_Caldat(area,Area).

%0603
add_Caldat(Key,Val) ->
    Row = #caldata{key=Key, val=Val},
    F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).

add_Maxno_item(Number) ->
    Row = #maxNo{name=scname, number=Number},
    F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).


add_scenario_item(Scname,Scenario)->
    F = fun() ->
		[Maxno] = mnesia:read({maxNo,scname}),
		Newno = Maxno#maxNo.number + 1 ,
		Maxno1 = Maxno#maxNo{number = Newno},
		mnesia:write(Maxno1),
		Scname1 = atom_to_list(Scname) ++ erlang:integer_to_list(Newno),
		%%scnario insert
   		Row = #scenario{scname=Scname1, scenario=Scenario},
		mnesia:write(Row)
	end,
    mnesia:transaction(F).


remove_scenario(Item) ->
    Oid = {scenario, Item},
    F = fun() ->
		mnesia:delete(Oid)
	end,
    mnesia:transaction(F).

get_scenariolist() ->
    do(qlc:q([X	|| X <- mnesia:table(scenario)])).




start_scenario_sav(Scname)->
	gen_server:cast({global,sim_db},{state_inf, start_scenario_sav,[scname,Scname]}).

stop_scenario_sav()->
	gen_server:cast({global,sim_db},{state_inf, stop_scenario_sav,[]}).

exe_scenario(Scname)->
	gen_server:cast({global,sim_db},{state_inf, exe_scenario,[scname,Scname]}).

%-------------------------------------
%% process state
%%-------------------------------------
%
event_loop(Mode,CreSc) ->
	receive
		{state_inf, start_scenario_sav,Parameters} when Mode == off ->
			Scname = getParam(scname,Parameters),
			put(scname,Scname),
			put(cnt,0),
			event_loop(on,[{space,{}}]);

		{state_inf, stop_scenario_sav,[]} when Mode =:= on ->
			Scname = erase(scname),
			add_scenario_item(Scname,CreSc),
			event_loop(off,[]);

		{state_inf,exe_scenario,Parameters} when Mode =:= off ->
			Scname = getParam(scname,Parameters),
			[Sc] = getDbValue(scenario,Scname),
			Scr = lists:reverse(Sc),
			lists:foreach(
				fun({To,Message})->
					if 
						To =:= b->
							{Cnt} = Message,
							ok = timer:sleep(?SAVTIME * Cnt);
						true ->
							gen_server:cast({global,To},Message),
							ok = timer:sleep(50)
					end
				end,
				Scr),
			event_loop(Mode,CreSc);


		{msgsav,To,Message} when Mode =:= on ->
%%io:format("sim_db : ~p~n", [Message ]),
			Sc2 = lists:append([{b,{get(cnt)}}], CreSc),
			put(cnt,0),
			Sc3 = lists:append([{To,Message}], Sc2),
			event_loop(Mode,Sc3);


		{'EXIT', Pid, Why} ->
			io:format("child process[~p] terminated : ~p~n", [Pid, Why]),
			exit(error);

		{timeout} ->
			if
				Mode =:= on ->
					put(cnt,get(cnt)+1),
					event_loop(Mode,CreSc);
				true->
					event_loop(Mode,CreSc)
			end;	

		_Any ->
%%			io:format("sim_db unknown type received : ~p,~p,~n", [_Any,Mode])
			event_loop(Mode,CreSc)

	end.

%-------------------------------------
%% tool
%%-------------------------------------
%%get parameter from message------
getParam(Key,[Hk,Hv|_]) when Key =:= Hk->
	Hv;
getParam(Key,[_,_|T])->
	getParam(Key,T);
getParam(_,[])->
	false.

%%TIMER
timer(PPid)->
	receive 
		cancel ->
			void
	after ?SAVTIME ->
		PPid ! {timeout},
		timer(PPid)
	end.
