#
# Makefile to simulate and synthesize VHDL designs
#
# @Author      Marc Eberhard/Richard Howe
# @Copyright   Copyright 2013 Marc Eberhard, 2016 Richard Howe
# @License     LGPL
#
# This file needs a lot of improving
#

NETLIST=top
#TIME=time -p 
TIME=

GUI_LDFLAGS = -lglut -lGL -lm 
CFLAGS      = -Wall -Wextra -O2 -g

.PHONY: simulation viewer synthesis bitfile upload clean

## Remember to update the synthesis section as well
SOURCES = \
	util.vhd \
	timer.vhd \
	bram.vhd \
	uart.vhd \
	debounce.vhd \
	kbd.vhd \
	vga.vhd \
	irqh.vhd \
	h2.vhd \
	cpu.vhd \
	gen.vhd \
	led.vhd 

OBJECTS = ${SOURCES:.vhd=.o}

all:
	@echo ""
	@echo "Simulation:"
	@echo ""
	@echo "make simulation     - simulate design"
	@echo "make viewer         - start waveform viewer for simulation results"
	@echo "make h2             - make experimental C simulator"
	@echo ""
	@echo "Synthesis:"
	@echo ""
	@echo "make synthesis      - synthesize design"
	@echo "make implementation - implement design"
	@echo "make bitfile        - generate bitfile"
	@echo ""
	@echo "Upload:"
	@echo ""
	@echo "make upload         - upload design to FPGA"
	@echo ""
	@echo "Cleanup:"
	@echo ""
	@echo "make clean          - delete temporary files and cleanup directory"
	@echo ""


## Assembler ===============================================================

%.hex: %.fth h2
	./h2 -S h2.sym -a $< > $@

## Virtual Machine and UART communications =================================

uart: uart.c
	${CC} ${CFLAGS} -std=gnu99 $^ -lpthread -o $@

h2: h2.c
	${CC} ${CFLAGS} -std=c99 $^ -o $@

disassemble: h2 h2.fth
	./h2 -S h2.sym -a h2.fth > h2.hex
	./h2 -L h2.sym h2.hex | awk '{printf "%04x %s\n", NR-1, $$0;}' | less -

run: h2 h2.fth
	./h2 -T -v -R h2.fth

sim.o: h2.c h2.h
	${CC} ${CFLAGS} -std=c99 -DNO_MAIN  $< -c -o $@

gui.o: gui.c
	${CC} ${CFLAGS} -std=gnu99  $< -c -o $@

gui: sim.o gui.o
	${CC} $^ ${GUI_LDFLAGS} -o $@

grun: gui h2.hex
	./$^

## Simulation ==============================================================

%.o: %.vhd
	ghdl -a $<

irqh.o: util.o
led.o: util.o led.vhd
gen.o: util.o gen.vhd
vga.o: bram.o vga.vhd text.bin font.bin
kbd.o: kbd.vhd debounce.o
cpu.o: util.o h2.o irqh.o bram.o cpu.vhd h2.hex
uart.o: util.o uart.vhd
top.o: util.o timer.o cpu.o uart.o vga.o kbd.o led.o top.vhd 
tb.o: top.o gen.o tb.vhd 

tb: ${OBJECTS} tb.o
	ghdl -e tb

%.ghw: % %.cfg
	ghdl -r $< --wave=$<.ghw

simulation: tb.ghw h2

## Simulation ==============================================================

viewer: simulation
	gtkwave -S signals -f tb.ghw &> /dev/null&

bitfile: design.bit

reports:
	@[ -d reports    ]    || mkdir reports
tmp:
	@[ -d tmp        ]    || mkdir tmp
tmp/_xmsgs:
	@[ -d tmp/_xmsgs ]    || mkdir tmp/_xmsgs

tmp/top.prj: tmp
	@rm -f tmp/top.prj
	@( \
	    for f in $(SOURCES); do \
	        echo "vhdl work \"$$f\""; \
	    done; \
	    echo "vhdl work \"top.vhd\"" \
	) > tmp/top.prj

tmp/top.lso: tmp
	@echo "work" > tmp/top.lso

tmp/top.xst: tmp tmp/_xmsgs tmp/top.lso tmp/top.lso
	@( \
	    echo "set -tmpdir \"tmp\""; \
	    echo "set -xsthdpdir \"tmp\""; \
	    echo "run"; \
	    echo "-lso tmp/top.lso"; \
	    echo "-ifn tmp/top.prj"; \
	    echo "-ofn top"; \
	    echo "-p xc6slx16-csg324-3"; \
	    echo "-top top"; \
	    echo "-opt_mode speed"; \
	    echo "-opt_level 2" \
	) > tmp/top.xst

synthesis: h2.hex reports tmp tmp/_xmsgs tmp/top.prj tmp/top.xst
	@echo "Synthesis running..."
	@${TIME} xst -intstyle silent -ifn tmp/top.xst -ofn reports/xst.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv top_xst.xrpt tmp
	@grep "ERROR\|WARNING" reports/xst.log | \
	 grep -v "WARNING.*has a constant value.*This FF/Latch will be trimmed during the optimization process." | \
	 cat

implementation: reports tmp
	@echo "Implementation running..."
	
	@[ -d tmp/xlnx_auto_0_xdb ] || mkdir tmp/xlnx_auto_0_xdb

	@${TIME} ngdbuild -intstyle silent -quiet -dd tmp -uc top.ucf -p xc6slx16-csg324-3 top.ngc top.ngd
	@mv top.bld reports/ngdbuild.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv xlnx_auto_0_xdb/* tmp
	@rmdir xlnx_auto_0_xdb
	@mv top_ngdbuild.xrpt tmp

	@${TIME} map -intstyle silent -detail -p xc6slx16-csg324-3 -pr b -c 100 -w -o top_map.ncd top.ngd top.pcf
	@mv top_map.mrp reports/map.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv top_usage.xml top_summary.xml top_map.map top_map.xrpt tmp

	@${TIME} par -intstyle silent -w -ol std top_map.ncd top.ncd top.pcf
	@mv top.par reports/par.log
	@mv top_pad.txt reports/par_pad.txt
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv par_usage_statistics.html top.ptwx top.pad top_pad.csv top.unroutes top.xpi top_par.xrpt tmp
	
	@#trce -intstyle silent -v 3 -s 3 -n 3 -fastpaths -xml top.twx top.ncd -o top.twr top.pcf -ucf top.ucf
	@#mv top.twr reports/trce.log
	@#mv _xmsgs/* tmp/_xmsgs
	@#rmdir _xmsgs
	@#mv top.twx tmp

	@#netgen -intstyle silent -ofmt vhdl -sim -w top.ngc top_xsim.vhd
	@#netgen -intstyle silent -ofmt vhdl -sim -w -pcf top.pcf top.ncd top_tsim.vhd
	@#mv _xmsgs/* tmp/_xmsgs
	@#rmdir _xmsgs
	@#mv top_xsim.nlf top_tsim.nlf tmp


design.bit: reports tmp/_xmsgs
	@echo "Generate bitfile running..."
	@touch webtalk.log
	@${TIME} bitgen -intstyle silent -w top.ncd
	@mv top.bit design.bit
	@mv top.bgn reports/bitgen.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@sleep 5
	@mv top.drc top_bitgen.xwbt top_usage.xml top_summary.xml webtalk.log tmp
	@grep -i '\(warning\|clock period\)' reports/xst.log

upload: 
	djtgcfg prog -d Nexys3 -i 0 -f design.bit

design: clean simulation synthesis implementation bitfile

postsyn:
	@netgen -w -ofmt vhdl -sim ${NETLIST}.ngc post_synthesis.vhd
	@netgen -w -ofmt vhdl -sim ${NETLIST}.ngd post_translate.vhd
	@netgen  -pcf ${NETLIST}.pcf -w -ofmt vhdl -sim ${NETLIST}.ncd post_map.vhd


clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *~ *.o trace.dat tb tb.ghw work-obj93.cf top.ngc top.ngd top_map.ngm \
	      top.pcf top_map.ncd top.ncd top_xsim.vhd top_tsim.vhd top_tsim.sdf \
	      top_tsim.nlf top_xst.xrpt top_ngdbuild.xrpt top_usage.xml top_summary.xml \
	      top_map.map top_map.xrpt par_usage_statistics.html top.ptwx top.pad top_pad.csv \
	      top.unroutes top.xpi top_par.xrpt top.twx top.nlf design.bit top_map.mrp 
	@rm -vrf _xmsgs reports tmp xlnx_auto_0_xdb
	@rm -vrf _xmsgs reports tmp xlnx_auto_0_xdb
	@rm -vrf h2 uart
	@rm -vf usage_statistics_webtalk.html
	@rm -vf mem_h2.binary mem_h2.hexadecimal
