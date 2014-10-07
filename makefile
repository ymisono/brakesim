SRCS=wheel_gui.erl wheel.erl sim_sup.erl sim_part.erl dcs.erl distance.erl distance_gui.erl throttle.erl throttle_gui.erl brake.erl brake_gui.erl output.erl output_gui.erl loadenv.erl sim_db.erl sim_app.erl balancectl.erl transmission.erl scenario.erl scenario_gui.erl loadenv_gui.erl calets.erl cruisectl.erl cruisectl_gui.erl

BEAMS=$(SRCS:.erl=.beam)

.SUFFIXES:.erl .beam
.erl.beam:
	erlc $<

all:$(BEAMS) 

.PHONY:clean
clean: $(BEAMS)
	rm $^

sn1:
	erl -name node1@127.0.0.1 -cookie 20140627

sn2:
	erl -name node2@127.0.0.1 -cookie 20140627
