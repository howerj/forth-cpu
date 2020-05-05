# Richard Howe
# TCL Script for GTKWave on tb.ghw 
#

set nfacs [ gtkwave::getNumFacs ]

gtkwave::/Edit/Set_Trace_Max_Hier 0
gtkwave::/Time/Zoom/Zoom_Amount -31.5

set instruction "top.tb.dbgi.insn"

set names [list]
lappend names "top.tb.clk"
lappend names "top.tb.rst"
lappend names "top.tb.dbgi.pc"
lappend names "$instruction"
lappend names "top.tb.dbgi.daddr"
lappend names "top.tb.dbgi.dout"
lappend names "top.tb.dbgi.din"
lappend names "top.tb.o_vga.hsync"
lappend names "top.tb.o_vga.vsync"

puts "signal list: $names"

set num_added [ gtkwave::addSignalsFromList $names ]
puts "num signals added: $num_added"

set highlight_insn [list]
lappend highlight_insn $instruction

set num_added [ gtkwave::highlightSignalsFromList "top.tb.dbgi.insn\[15:0\]"]
puts "num highlighted: $num_added"
gtkwave::/Edit/Highlight_Regexp "insn"
set procFile "./h2"
set OS [lindex $tcl_platform(os) 0]
if { $OS == "Windows" } {
	set procFile "h2.exe"
} 

set which_f [ gtkwave::setCurrentTranslateProc $procFile ]
puts "which_f: $which_f"
gtkwave::/Edit/Data_Format/Translate_Filter_Process/Enable_and_Select

