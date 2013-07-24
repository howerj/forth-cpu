#
# Makefile to simulate and synthesize VHDL designs
#
# @Author 		Marc Eberhard/Richard Howe
# @Copyright	Copyright 2013 Marc Eberhard
# @License		LGPL
#


## Remember to update the synthesis section as well
SOURCES = \
	mem_font.vhd \
	mem_text.vhd \
	uart.vhd \
	ctrm.vhd \
	losr.vhd \
	vga80x40.vhd \
	vga_top.vhd \
	h2.vhd \
	mem_h2.vhd \
	io.vhd \
	cpu.vhd \

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

##
## Simulation ==============================================================
an_mem_font: mem_font.vhd
	@echo "ghdl -a mem_font.vhd"
	@ghdl -a mem_font.vhd

an_mem_text: mem_text.vhd
	@echo "ghdl -a mem_text.vhd"
	@ghdl -a mem_text.vhd

an_ctrm: ctrm.vhd
	@echo "ghdl -a ctrm.vhd"
	@ghdl -a ctrm.vhd

an_losr: losr.vhd
	@echo "ghdl -a losr.vhd"
	@ghdl -a losr.vhd

an_vga80x40: an_ctrm an_losr vga80x40.vhd
	@echo "ghdl -a vga80x40.vhd"
	@ghdl -a vga80x40.vhd

an_vga_top: an_mem_font an_mem_text an_vga80x40 vga_top.vhd
	@echo "ghdl -a vga_top.vhd"
	@ghdl -a vga_top.vhd

an_uart: uart.vhd
	@echo "ghdl -a uart.vhd"
	@ghdl -a uart.vhd

an_h2: h2.vhd
	@echo "ghdl -a h2.vhd"
	@ghdl -a h2.vhd

an_mem_h2: mem_h2.vhd
	@echo "ghdl -a mem_h2.vhd"
	@ghdl -a mem_h2.vhd

an_cpu: an_h2 an_mem_h2 cpu.vhd
	@echo "ghdl -a cpu.vhd"
	@ghdl -a cpu.vhd

simulation: an_cpu an_uart an_vga_top top_level.vhd test_bench.vhd
	ghdl -a top_level.vhd
	ghdl -a test_bench.vhd
	ghdl -e test_bench
	ghdl -r test_bench --wave=test_bench.ghw 
## Simulation ==============================================================

viewer:
	gtkwave test_bench.ghw &>/dev/null&

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

upload:
	djtgcfg prog -d Nexys3 -i 0 -f design.bit

design: clean synthesis implementation bifile

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -f *~ *.o trace.dat test_bench test_bench.ghw work-obj93.cf top_level.ngc top_level.ngd top_level_map.ngm \
	      top_level.pcf top_level_map.ncd top_level.ncd top_level_xsim.vhd top_level_tsim.vhd top_level_tsim.sdf \
	      top_level_tsim.nlf top_level_xst.xrpt top_level_ngdbuild.xrpt top_level_usage.xml top_level_summary.xml \
	      top_level_map.map top_level_map.xrpt par_usage_statistics.html top_level.ptwx top_level.pad top_level_pad.csv \
	      top_level.unroutes top_level.xpi top_level_par.xrpt top_level.twx top_level.nlf design.bit
	@rm -rf _xmsgs reports tmp xlnx_auto_0_xdb
	@rm -f usage_statistics_webtalk.html
