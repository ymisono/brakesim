%%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/11/01
%%% -------------------------------------------------------------------
-module(loadenv_gui).

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
-define(LIST_TITLE, ['0.7','0.5','0.15']).
-define(VALUE, [0.7,0.5,0.15]).
-define(AREA, "1.9").
-define(INIVAL, 0).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start(Parent)->
	put(area,?AREA),
	put(frictional,lists:nth(?INIVAL+1,?VALUE)),
	GSHandle = gs:start(),
	{WinX, WinY} = {800, 30},
	WinHandle = gs:create(window, GSHandle,
						  [
						   {title, "Load environment"},
						   {width, 200},
						   {height,200},
						   {x, WinX},
						   {y, WinY},
						   {bg, black},
						   {keypress, true}]),
	put(mainWindow, WinHandle),
	Lb=gs:listbox(WinHandle,[{x,5},{y,15},{width,160},{height,85},{vscroll,right},{click,true},{doubleclick,true}]),
	gs:config(Lb,[{items,?LIST_TITLE},{selection,?INIVAL}]),

	put(quitButton, gs:create(button, quit, WinHandle, [{label, {text,"quit"}} ,{width,80},{height,25}, {x,10},{y,170}])),
	gs:create(label, WinHandle, [{x, 0}, {y, 3}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 10}, {width, 200}, {label, {text, "frictional properties"}}, {font, {screen,bold, 12}}]),	
	put(areawd,gs:create(label, WinHandle, [{x, 0}, {y, 113}, {align, w}, {bw, 1}, {fg, red}, {bg, black}, {height, 15}, {width, 200}, {label, {text, "Area of wind drag[m^2] = " ++ get(area)}}, {font, {screen,bold, 12}}])),
	gs:config(WinHandle, {map, true}),
	put(parentPid, Parent),
	event_loop(),
	ok.



%% ====================================================================
%% Internal functions
%% ====================================================================
event_loop() ->
	receive
		{gs, quit, _, _, _} ->
			gs:destroy(get(mainWindow));
		{gs, _, destroy, _, _} ->
			true;

		{gs,_,click,_,[Idx,_|_]}->
			Val = lists:nth(Idx+1,?VALUE),
			get(parentPid)!{state_inf,chgui,[frictional,Val]},
			event_loop();

		{state_inf,pare,[area,Val]} ->
			if
				is_integer(Val) ->
					Text = "Area of wind drag[m^2] =  " ++ integer_to_list(Val);
				is_float(Val) ->
					Text = "Area of wind drag[m^2] =  " ++ float_to_list(Val);
				is_atom(Val) ->
					Text = "Area of wind drag[m^2] =  " ++ atom_to_list(Val);
				true->
					Text = "Area of wind drag[m^2] =  " ++ Val
			end,
			gs:config(get(areawd), {label,{text,Text }}),
			event_loop();
		Any ->
			erlang:display(Any),
			event_loop()
	end.

