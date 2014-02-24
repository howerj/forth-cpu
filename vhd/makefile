#
# Makefile to simulate and synthesize VHDL designs
#
# @Author 		Marc Eberhard/Richard Howe
# @Copyright	Copyright 2013 Marc Eberhard
# @License		LGPL
#

DOXYFILE=doxygen.conf
NETLIST=top_level

## Remember to update the synthesis section as well
SOURCES = \
	util.vhd \
	uart.vhd \
	ps2.vhd \
	vga80x40.vhd \
	vga_top.vhd \
	h2.vhd \
	cpu.vhd 

all:
	@echo ""
	@echo "Simulation:"
	@echo ""
	@echo "make simulation     - simulate design"
	@echo "make viewer         - start waveform viewer for simulation results"
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

##
## Simulation ==============================================================
util.o: util.vhd
	@echo "ghdl -a util.vhd"
	@ghdl -a util.vhd

vga80x40.o: util.o vga80x40.vhd
	@echo "ghdl -a vga80x40.vhd"
	@ghdl -a vga80x40.vhd

vga_top.o: util.o vga80x40.o vga_top.vhd mem_text.binary mem_font.binary
	@echo "ghdl -a vga_top.vhd"
	@ghdl -a vga_top.vhd

ps2.o: ps2.vhd
	@echo "ghdl -a ps2.vhd"
	@ghdl -a ps2.vhd

uart.o: uart.vhd
	@echo "ghdl -a uart.vhd"
	@ghdl -a uart.vhd

h2.o: h2.vhd
	@echo "ghdl -a h2.vhd"
	@ghdl -a h2.vhd

cpu.o: h2.o util.o cpu.vhd mem_h2.hexadecimal
	@echo "ghdl -a cpu.vhd"
	@ghdl -a cpu.vhd

top_level.o: util.o cpu.o uart.o vga_top.o ps2.o top_level.vhd
	@echo "ghdl -a top_level.vhd"
	@ghdl -a top_level.vhd

test_bench.o: top_level.o test_bench.vhd
	@echo "ghdl -a test_bench.vhd"
	@ghdl -a test_bench.vhd

simulation: test_bench.o
	ghdl -e test_bench
	ghdl -r test_bench --wave=test_bench.ghw 
## Simulation ==============================================================

viewer:
	gtkwave -f test_bench.ghw; 
	# gtkwave -S gtkwave.tcl -f test_bench.ghw &> /dev/null&

synthesis:
	@echo "Synthesis running..."

	@[ -d reports    ]    || mkdir reports
	@[ -d tmp        ]    || mkdir tmp
	@[ -d tmp/_xmsgs ]    || mkdir tmp/_xmsgs
	
	@echo "work" > tmp/top_level.lso

	@( \
	    for f in $(SOURCES); do \
	        echo "vhdl work \"$$f\""; \
	    done; \
	    echo "vhdl work \"top_level.vhd\"" \
	) > tmp/top_level.prj

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
	    echo "-opt_level 1" \
	) > tmp/top_level.xst

	@xst -intstyle silent -ifn tmp/top_level.xst -ofn reports/xst.log
	@mv _xmsgs/* tmp/_xmsgs
	@rmdir _xmsgs
	@mv top_level_xst.xrpt tmp
	@grep "ERROR\|WARNING" reports/xst.log | \
	 grep -v "WARNING.*has a constant value.*This FF/Latch will be trimmed during the optimization process." | \
	 cat

implementation: 
	@echo "Implementation running..."
	
	@[ -d reports             ] || mkdir reports
	@[ -d tmp                 ] || mkdir tmp
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

bitfile:
	@echo "Generate bitfile running..."
	@touch webtalk.log
	@bitgen -intstyle silent -w top_level.ncd
	@[ -d reports ] || mkdir reports
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
	@rm -vrf doxy/
	@rm -vf usage_statistics_webtalk.html
