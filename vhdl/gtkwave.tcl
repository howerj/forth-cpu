# Richard Howe
# TCL Script for GTKWave on test_bench.ghw for final year project 
#

set nfacs [ gtkwave::getNumFacs ]
set dumpname [ gtkwave::getDumpFileName ]
set dmt [ gtkwave::getDumpType ]

puts "number of signals in dumpfile '$dumpname' of type $dmt: $nfacs"

set names [list]
lappend names "tb_clk"
lappend names "tb_rst"
lappend names "pc_c"
lappend names "insn"
lappend names "tos_c"
lappend names "nos"
lappend names "vstkp_c"

set num_added [ gtkwave::addSignalsFromList $names ]
puts "num signals added: $num_added"

set highlight_insn [list]

for {set i 0} {$i < $nfacs } {incr i} {
     set facname [ gtkwave::getFacName $i ]
     set indx [ string last "insn" $facname  ]
 
     if {$indx != -1} {
       lappend highlight_insn "$facname"
       puts "found : $facname"
   }
}

set num_added [ gtkwave::highlightSignalsFromList $highlight_insn ]
puts "num highlighted: $num_added"
gtkwave::/Edit/Highlight_Regexp "insn"
set procFile "../test/filter/filter"
set which_f [ gtkwave::setCurrentTranslateProc $procFile ]
gtkwave::/Edit/Data_Format/Translate_Filter_Process/Enable_and_Select
puts "which_f: $which_f"
## Causes seg fault, is this the right command?
#set num_update [ gtkwave::installProcFilter $which_f ]
#puts "num updated with proc filter: $num_update"

gtkwave::/Edit/Set_Trace_Max_Hier 0
gtkwave::/Time/Zoom/Zoom_Amount -21.5
# zoom amount - 21.5

#gtkwave::setMarker 128
#gtkwave::setNamedMarker A 400 "Example Named Marker"

#gtkwave::/File/Print_To_File PS {Letter (8.5" x 11")} Full $dumpname.ps
#gtkwave::/File/Quit
