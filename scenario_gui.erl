%%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/11/01
%%% -------------------------------------------------------------------
-module(scenario_gui).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/1,chkFileUpdate/1,creOkNgWind/0]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(LIST_TITLE, ['abcefg1','abcefg2','abcefg3']).
-define(SCDIR,"scenario").
-define(LOGDIR,"log").

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start(Parent)->
	put(selectsc,'null'),
	put(scstate,'idle'),

	GSHandle = gs:start(),
	{WinX, WinY} = {800, 30},
	Win = gs:create(window, GSHandle,
						  [
						   {title, "Scenario & Log"},
						   {width, 400},
						   {height,320},
						   {x, WinX},
						   {y, WinY},
						   {keypress, true}]),
	put(mainWindow, Win),
	gs:create(label,Win,[{label,{text,"Add scenario name"}},{width,100}]),
	gs:create(entry,newsc,Win,[{x,10},{y,25},{width,140},{keypress,true}]),
	put(savebot,gs:create(button,save,Win,[{width,50},{y,25},{x,160},{label,{text,"save"}}])),
	ExeBot = gs:create(button,exe,Win,[{width,50},{y,60},{x,160},{label,{text,"execute"}}]),
	put(exebot,ExeBot),
	put(selectsclabel,gs:create(label,Win,[{label,{text,atom_to_list(get(selectsc))}},{width,100},{y,60},{fg, red},{font, {screen,bold, 12}}])),
	gs:create(label,Win,[{label,{text,"Scenario Status"}},{width,100},{y,90}]),
	put(scstateobj,gs:create(label,Win,[{label,{text,atom_to_list(get(scstate))}},{width,180},{y,110}, {fg, red},{font, {screen,bold, 12}}])),

	gs:create(label,Win,[{label,{text,"ScenarioList"}},{width,80},{x,210}]),
	Lb=gs:listbox(Win,[{x,220},{y,25},{width,160},{height,260},{vscroll,right},{click,true},{doubleclick,true}]),
	put(listbox,Lb),
	gs:config(Lb,[{items,getScList()}]),

	gs:create(label,Win,[{label,{text,"New Log name"}},{width,100},{y,160}]),
	gs:create(entry,newlog,Win,[{x,10},{y,185},{width,140},{keypress,true}]),
	LogBot = gs:create(button,log,Win,[{width,50},{y,185},{x,160},{label,{text,"Log Save"}}]),
	put(logbot,LogBot),
	gs:create(label,Win,[{label,{text,"Logging Status"}},{width,100},{y,245}]),
	put(logstateobj,gs:create(label,Win,[{label,{text,"idle"}},{width,180},{y,270}, {fg, red},{font, {screen,bold, 12}}])),


	gs:config(Win,{map,true}),
	put(parentPid, Parent),
	event_loop(Lb,idle),
	ok.



%% ====================================================================
%% Internal functions
%% ====================================================================
event_loop(Lb,Logstate) ->
	receive
		{gs, quit, _, _, _} ->
			gs:destroy(get(mainWindow));
		{gs, _, destroy, _, _} ->
			true;
		{gs,save,click,_,_}->
			Text=gs:read(newsc,text),
			scctl(save,Text,get(scstate)),
			event_loop(Lb,Logstate);
		{gs,exe,click,_,_}->
			scctl(exe,get(selectsc),get(scstate)),
			event_loop(Lb,Logstate);

		{gs,log,click,_,_} when Logstate =:= idle->
			Text=gs:read(newlog,text),
			case chkFileUpdate(Text) of
				none -> 
					false;
				false -> 
					creOkNgWind();
				true ->
					logstart(Text)
			end,
			event_loop(Lb,logging);

		{gs,log,click,_,_} ->
			logstop(),
			event_loop(Lb,idle);

		%sclist select
		{gs,Lb,click,_,[Idx,_|_]}->
			put(selectsc,lists:nth(Idx+1,getScList())),
			gs:config(get(selectsclabel), {label,{text,get(selectsc)}}),
			event_loop(Lb,Logstate);

		%popup update
		{gs,logupdate,click,_,_} ->
			Text=gs:read(newlog,text),
			logstart(Text),
			gs:destroy(get(popup)),
			event_loop(Lb,logging);
			

		%popup cancel
		{gs,logcancel,click,_,_} ->
			gs:destroy(get(popup)),
			event_loop(Lb,idle);

		_Any ->
		%	erlang:display(Any),
			event_loop(Lb,Logstate)
	end.

scctl(_Eve,Text,_State) when length(Text) =:= 0->
	false;

scctl(Eve,Text,State) when State =:= idle,Eve =:= save->
	put(scstate,'saving'),
	get(parentPid)!{scsave,chgui,[scname,?SCDIR ++ "/" ++ Text]},
	gs:config(get(savebot),{label,{text,"stop"}}),
	gs:config(get(scstateobj), {label,{text, Text ++ " "++atom_to_list(get(scstate))}} );

scctl(Eve,_Text,State) when State =:= saving,Eve =:= save->
	put(scstate,'idle'),
	get(parentPid)!{scstop,chgui,[]},
	gs:config(get(savebot),{label,{text,"save"}}),
	gs:config(get(listbox),[{items,getScList()}]),
	gs:config(get(scstateobj), {label,{text,atom_to_list(get(scstate))}} );

scctl(Eve,Text,State) when State =:= idle,Eve =:=exe->
	put(scstate,'executing'),
	get(parentPid)!{scexe,chgui,[scname,?SCDIR ++ "/" ++ Text]},
	gs:config(get(exebot),{label,{text,"stop"}}),
	gs:config(get(scstateobj), {label,{text, Text ++ " "++atom_to_list(get(scstate))}} );

scctl(Eve,_,State) when State =:= executing,Eve =:=exe->
	put(scstate,'idle'),
	get(parentPid)!{scexestop,chgui,[]},
	gs:config(get(exebot),{label,{text,"execute"}}),
	gs:config(get(scstateobj), {label,{text,atom_to_list(get(scstate))}} );

scctl(_Eve,_Text,_State) ->
	false.


logstart(Text)->
	get(parentPid)!{logstart,chgui,[logname,?LOGDIR ++ "/" ++ Text]},
	gs:config(get(logbot),{label,{text,"Log Stop"}}),
	gs:config(get(logstateobj), {label,{text,Text ++ " logging"}} ).

logstop()->
	get(parentPid)!{logstop,chgui,[]},
	gs:config(get(logbot),{label,{text,"Log Save"}}),
	gs:config(get(logstateobj), {label,{text,"idle"}} ).


getScList()->
	{ok,FiList} = file:list_dir(?SCDIR),
	FiList.



chkFileUpdate(File) when length(File) =:= 0->
	none;
chkFileUpdate(File)->
	case file:read_file_info(?LOGDIR++"/"++File) of
		{ok,_}-> 
			false;
		_->true
	end. 

creOkNgWind()->
	Canvas = gs:create(canvas, get(mainWindow), [{x, 20},{y, 160},{width, 200},{height, 100},{bg,black}]),
	put(popup,Canvas),
	gs:create(label,Canvas,[{label,{text,"Already exists"}},{width,100},{x,5},{y,5},{bg,black},{fg, red}]),
	gs:create(button,logupdate,Canvas,[{width,50},{y,40},{x,25},{label,{text,"update?"}}]),
	gs:create(button,logcancel,Canvas,[{width,50},{y,40},{x,100},{label,{text,"cancel?"}}]).



