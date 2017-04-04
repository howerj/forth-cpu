#
# Makefile to simulate and synthesize VHDL designs
#
# @Author      Marc Eberhard/Richard Howe
# @Copyright   Copyright 2013 Marc Eberhard, 2016 Richard Howe
# @License     LGPL
#
# This file needs a lot of improving
#

DOXYFILE=doxygen.conf
NETLIST=top_level

.PHONY: simulation viewer synthesis bitfile upload clean

## Remember to update the synthesis section as well
SOURCES = \
	util.vhd \
	uart.vhd \
	edge_detector_rising.vhd \
	debounce.vhd \
	ps2_keyboard.vhd \
	ps2_keyboard_to_ascii.vhd \
	vga80x40.vhd \
	vga_top.vhd \
	irqh.vhd \
	h2.vhd \
	cpu.vhd \
	ledseg.vhd \

OBJECTS = $(SOURCES:.vhd=.o)

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
	@echo "make doxygen"       - make doxygen documentation
	@echo ""


## Assembler ===============================================================

mem_h2.hexadecimal: assembler cpu.asm
	./assembler

## Virtual Machine =========================================================

h2: h2.c
	${CC} -Wall -Wextra -g -std=c99 $^ -o $@

## Simulation ==============================================================

filter: filter.c
	$(CC) $^ -o $@

%.o: %.vhd
	ghdl -a $<

vga80x40: util.o
vga_top.o: util.o vga80x40.o vga_top.vhd mem_text.binary mem_font.binary
ps2_keyboard.o: ps2_keyboard.vhd debounce.o
ps2_keyboard_to_ascii.o: ps2_keyboard_to_ascii.vhd ps2_keyboard.o debounce.o
cpu.o: h2.o irqh.o util.o cpu.vhd mem_h2.hexadecimal
top_level.o: util.o cpu.o uart.o vga_top.o ps2_keyboard_to_ascii.o ledseg.o top_level.vhd 
test_bench.o: top_level.o test_bench.vhd

test_bench: $(OBJECTS) test_bench.o
	ghdl -e test_bench

%.ghw: %
	ghdl -r $^ --wave=$^.ghw

simulation: test_bench.ghw

## Simulation ==============================================================

viewer: filter simulation
	gtkwave -S gtkwave.tcl -f test_bench.ghw &> /dev/null&

bitfile: design.bit

reports:
	@[ -d reports    ]    || mkdir reports
tmp:
	@[ -d tmp        ]    || mkdir tmp
tmp/_xmsgs:
	@[ -d tmp/_xmsgs ]    || mkdir tmp/_xmsgs

tmp/top_level.prj: tmp
	@rm -f tmp/top_level.prj
	@( \
	    for f in $(SOURCES); do \
	        echo "vhdl work \"$$f\""; \
	    done; \
	    echo "vhdl work \"top_level.vhd\"" \
	) > tmp/top_level.prj

tmp/top_level.lso: tmp
	@echo "work" > tmp/top_level.lso

tmp/top_level.xst: tmp tmp/_xmsgs tmp/top_level.lso tmp/top_level.lso
	@( \
	    echo "set -tmpdir \"tmp\""; \
	    echo "set -xsthdpdir \"tmp\""; \
	    echo "run"; \
	    echo "-lso tmp/top_level.lso"; \
	    echo "-ifn tmp/top_level.prj"; \
	    echo "-ofn top_level"; \
	    echo "-p xc6slx16-csg324-3"; \
	    echo "-top top_level"; \
	    echo "-opt_mode speed"; \
	    echo "-opt_level 2" \
	) > tmp/top_level.xst

synthesis: reports tmp tmp/_xmsgs tmp/top_level.prj tmp/top_level.xst
	@echo "Synthesis running..."
	@xst -intstyle silent -ifn tmp/top_level.xst -ofn reports/xst.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv top_level_xst.xrpt tmp
	@grep "ERROR\|WARNING" reports/xst.log | \
	 grep -v "WARNING.*has a constant value.*This FF/Latch will be trimmed during the optimization process." | \
	 cat

implementation: reports tmp
	@echo "Implementation running..."
	
	@[ -d tmp/xlnx_auto_0_xdb ] || mkdir tmp/xlnx_auto_0_xdb

	@ngdbuild -intstyle silent -quiet -dd tmp -uc top_level.ucf -p xc6slx16-csg324-3 top_level.ngc top_level.ngd
	@mv top_level.bld reports/ngdbuild.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv xlnx_auto_0_xdb/* tmp
	@rmdir xlnx_auto_0_xdb
	@mv top_level_ngdbuild.xrpt tmp

	@map -intstyle silent -detail -p xc6slx16-csg324-3 -pr b -c 100 -w -o top_level_map.ncd top_level.ngd top_level.pcf
	@mv top_level_map.mrp reports/map.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv top_level_usage.xml top_level_summary.xml top_level_map.map top_level_map.xrpt tmp

	@par -intstyle silent -w -ol std top_level_map.ncd top_level.ncd top_level.pcf
	@mv top_level.par reports/par.log
	@mv top_level_pad.txt reports/par_pad.txt
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv par_usage_statistics.html top_level.ptwx top_level.pad top_level_pad.csv top_level.unroutes top_level.xpi top_level_par.xrpt tmp
	
	@#trce -intstyle silent -v 3 -s 3 -n 3 -fastpaths -xml top_level.twx top_level.ncd -o top_level.twr top_level.pcf -ucf top_level.ucf
	@#mv top_level.twr reports/trce.log
	@#mv _xmsgs/* tmp/_xmsgs
	@#rmdir _xmsgs
	@#mv top_level.twx tmp

	@#netgen -intstyle silent -ofmt vhdl -sim -w top_level.ngc top_level_xsim.vhd
	@#netgen -intstyle silent -ofmt vhdl -sim -w -pcf top_level.pcf top_level.ncd top_level_tsim.vhd
	@#mv _xmsgs/* tmp/_xmsgs
	@#rmdir _xmsgs
	@#mv top_level_xsim.nlf top_level_tsim.nlf tmp


design.bit: reports tmp/_xmsgs
	@echo "Generate bitfile running..."
	@touch webtalk.log
	@bitgen -intstyle silent -w top_level.ncd
	@mv top_level.bit design.bit
	@mv top_level.bgn reports/bitgen.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@sleep 5
	@mv top_level.drc top_level_bitgen.xwbt top_level_usage.xml top_level_summary.xml webtalk.log tmp
	@grep -i '\(warning\|clock period\)' reports/xst.log

upload: 
	djtgcfg prog -d Nexys3 -i 0 -f design.bit

design: clean simulation synthesis implementation bitfile

postsyn:
	@netgen -w -ofmt vhdl -sim $(NETLIST).ngc post_synthesis.vhd
	@netgen -w -ofmt vhdl -sim $(NETLIST).ngd post_translate.vhd
	@netgen  -pcf $(NETLIST).pcf -w -ofmt vhdl -sim $(NETLIST).ncd post_map.vhd


doxygen: $(DOXYFILE)
	@doxygen $(DOXYFILE)

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *~ *.o trace.dat test_bench test_bench.ghw work-obj93.cf top_level.ngc top_level.ngd top_level_map.ngm \
	      top_level.pcf top_level_map.ncd top_level.ncd top_level_xsim.vhd top_level_tsim.vhd top_level_tsim.sdf \
	      top_level_tsim.nlf top_level_xst.xrpt top_level_ngdbuild.xrpt top_level_usage.xml top_level_summary.xml \
	      top_level_map.map top_level_map.xrpt par_usage_statistics.html top_level.ptwx top_level.pad top_level_pad.csv \
	      top_level.unroutes top_level.xpi top_level_par.xrpt top_level.twx top_level.nlf design.bit top_level_map.mrp 
	@rm -vrf _xmsgs reports tmp xlnx_auto_0_xdb
	@rm -vrf _xmsgs reports tmp xlnx_auto_0_xdb
	@rm -vrf doxy/ filter
	@rm -vf usage_statistics_webtalk.html
	@rm -vf mem_h2.binary mem_h2.hexadecimal
