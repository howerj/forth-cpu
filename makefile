#
# Makefile to simulate and synthesize VHDL designs
#
# @Author      Marc Eberhard/Richard Howe
# @Copyright   Copyright 2013 Marc Eberhard, 2016 Richard Howe
# @License     LGPL
#
# This makefile can build the toolchain, simulators, and the bit
# file for the FPGA. Type "make help" at the command line for a
# list of options
#

NETLIST=top
CFLAGS=-Wall -Wextra -O2 -g -pedantic
CC=gcc
TIME=
#TIME=time -p 

OS_FLAGS =
# From: https://stackoverflow.com/questions/714100/os-detecting-makefile
ifeq ($(OS),Windows_NT)
GUI_LDFLAGS = -lfreeglut -lopengl32 -lm 
DF=
EXE=.exe

.PHONY: h2 gui text block

h2:     h2.exe
gui:    gui.exe
text:   text.exe
block:  block.exe

else # assume unixen
GUI_LDFLAGS = -lglut -lGL -lm 
DF=./
EXE=
endif

.PHONY: simulation viewer synthesis bitfile upload clean run gui-run 

## Remember to update the synthesis section as well
SOURCES = \
	util.vhd \
	timer.vhd \
	uart.vhd \
	kbd.vhd \
	vga.vhd \
	h2.vhd \
	ram.vhd \
	core.vhd

OBJECTS = ${SOURCES:.vhd=.o}

all:
	@echo ""
	@echo "Simulation:"
	@echo ""
	@echo "make simulation     - simulate VHDL design"
	@echo "make viewer         - start waveform viewer for simulation results"
	@echo "make documentation  - build the PDF and HTML documentation"
	@echo "make h2${EXE}             - build C based CLI emulator for the VHDL SoC"
	@echo "make gui${EXE}            - build C based GUI emulator for the Nexys3 board"
	@echo "make run            - run the C CLI emulator on h2.fth"
	@echo "make gui-run        - run the GUI emulator on ${EFORTH}"
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

## Documentation ===========================================================

documentation: readme.pdf readme.htm

%.pdf: %.md
	pandoc -V geometry:margin=0.5in --toc $< -o $@

%.htm: %.md
	pandoc --toc --self-contained $^ -o $@

## Assembler, Virtual Machine and UART communications ======================

EFORTH=h2.hex

h2${EXE}: h2.c h2.h
	${CC} ${CFLAGS} -std=c99 $< -o $@

embed${EXE}: embed.o

${EFORTH}: embed${EXE} embed.blk embed.fth
	${DF}embed${EXE} embed.blk $@ embed.fth

block${EXE}: block.c
	${CC} ${CFLAGS} -std=c99 $< -o $@

nvram.blk: nvram.txt block${EXE} 
	${DF}block${EXE} < nvram.txt >  $@

run: h2${EXE} ${EFORTH} text.hex nvram.blk
	${DF}h2 -H -r ${EFORTH}

h2nomain.o: h2.c h2.h
	${CC} ${CFLAGS} -std=c99 -DNO_MAIN  $< -c -o $@

gui.o: gui.c h2.h
	${CC} ${CFLAGS} -std=gnu99  $< -c -o $@

gui${EXE}: h2nomain.o gui.o
	${CC} ${CFLAGS} $^ ${GUI_LDFLAGS} -o $@

gui-run: gui${EXE} ${EFORTH} nvram.blk text.hex
	${DF}$< ${EFORTH}

text${EXE}: text.c
	${CC} ${CFLAGS} -std=c99 $< -o $@

text.hex: text${EXE}
	${DF}$< -g > $@

## Simulation ==============================================================

%.o: %.vhd
	ghdl -a -g $<

ram.o: util.o
kbd.o: util.o kbd.vhd
vga.o: util.o vga.vhd text.hex font.bin
core.o: util.o h2.o core.vhd ${EFORTH}
uart.o: util.o uart.vhd
timer.o: util.o
top.o: util.o timer.o core.o uart.o vga.o kbd.o ram.o top.vhd  
tb.o: top.o util.o tb.vhd 

tb: ${OBJECTS} tb.o
	ghdl -e tb

# max stack alloc needed for GHDL >0.35
# ghdl -r $< --wave=$<.ghw --max-stack-alloc=16384 --unbuffered --ieee-asserts=disable 
%.ghw: % %.cfg
	ghdl -r $< --wave=$<.ghw --max-stack-alloc=16384 --ieee-asserts=disable --unbuffered

simulation: tb.ghw h2${EXE}

## Simulation ==============================================================


# gtkwave -S signals -f tb.ghw &> /dev/null&

ifeq ($(OS),Windows_NT)
viewer: simulation
	gtkwave -S signals -f tb.ghw 
else
viewer: simulation
	gtkwave -S signals -f tb.ghw &> /dev/null&
endif

USB?=/dev/ttyUSB0
BAUD?=115200
#BAUD?=9600

talk:
	picocom --omap delbs -e b -b ${BAUD} ${USB}

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

synthesis: ${EFORTH} text.hex reports tmp tmp/_xmsgs tmp/top.prj tmp/top.xst
	@echo "Synthesis running..."
	@${TIME} xst -intstyle silent -ifn tmp/top.xst -ofn reports/xst.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv top_xst.xrpt tmp
	@grep "ERROR\|WARNING" reports/xst.log | \
	 grep -v "WARNING.*has a constant value.*This FF/Latch will be trimmed during the optimization process." | \
	 cat
	@grep ns reports/xst.log | grep 'Clock period'

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
	@rm -vrf h2${EXE} gui${EXE} block${EXE} text${EXE}
	@rm -vrf text.bin ${EFORTH} text.hex
	@rm -vrf *.pdf *.htm
	@rm -vrf *.sym
	@rm -vrf xst/
	@rm -vf usage_statistics_webtalk.html

