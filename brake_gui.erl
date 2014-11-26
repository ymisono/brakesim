%%%% -------------------------------------------------------------------
%%% Author  : Masahiko KAGAWA
%%% Description :
%%%
%%% Created : 2010/05/07
%%% -------------------------------------------------------------------
-module(brake_gui).

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
start(Parent) ->
	GSHandle = gs:start(),
	{WinX, WinY} = {800, 500},
	WinHandle = gs:create(window, GSHandle,
						  [
						   {title, "Brake Pedal"},
						   {width, 230},
						   {height,200},
						   {x, WinX},
						   {y, WinY},
						   {bg, black},
						   {keypress, true}]),
	put(mainWindow, WinHandle),
	gs:create(label, WinHandle, [{x, 10}, {y, 5}, {align, w}, {bw, 1}, {fg, green}, {bg, black}, {height, 20}, {width, 100}, {label, {text, "[Parameters]"}}, {font, {screen, bold, 10}}]),
	put(pedalScale, gs:create(scale, WinHandle, [{x, 10}, {y, 20}, {orient, horizontal}, {fg, green}, {bg, black}, {pos, 0}, {range, {0, 100000}}, {showvalue, true}, {text, "Pedal Angle"}, {data, pedal}, {pos, 0}, {buttonpress, true}, {buttonrelease, true},{width, 200}])),
	put(quitButton, gs:create(button, quit, WinHandle, [{label, {text,"quit"}} ,{width,80},{height,25}, {x,10},{y,170}])),
	put(parentPid, Parent),
	gs:config(WinHandle, {map, true}),
	event_loop(none),
	ok.



%% ====================================================================
%% Internal functions
%% ====================================================================
event_loop(Option) ->
	receive
		{gs, quit, _, _, _} ->
			gs:destroy(get(mainWindow));
		{gs, _, destroy, _, _} ->
			true;
		{gs, _, click, pedal, _} when Option=/= none->
			event_loop(Option);
		{gs, _, click, pedal, [Angle|_]} ->
%			io:format("click : ~p ~n", [Angle]),
			PPid = get(parentPid),
			if 
				is_pid(PPid)-> 
					PPid!{state_inf,chgui,Angle};
				true -> true
			end,
			event_loop(Option);

		{state_inf,parent,Toru} ->
%			io:format("parent : ~p ~n", [Toru]),
			event_loop(Toru);

		_Any ->
%%			erlang:display(_Any),
			event_loop(Option)
	after 500   ->
		if
			Option =/= none ->
				gs:config(get(pedalScale),{pos, Option} );
			true ->
				false
		end,
		event_loop(none)
	end.

