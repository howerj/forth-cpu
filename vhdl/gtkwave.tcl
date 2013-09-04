#
# simple example of using tcl with gtkwave:
# 1) query the dumpfile for signals with "clk" or
#    [1:48] in the signal name
# 2) show full signal hierarchy
# 3) zoom full
# 4) set marker to 128 units
# 5) generate the postscript to a file
#

set nfacs [ gtkwave::getNumFacs ]
set dumpname [ gtkwave::getDumpFileName ]
set dmt [ gtkwave::getDumpType ]

puts "number of signals in dumpfile '$dumpname' of type $dmt: $nfacs"



#set clk48 [list]
# for {set i 0} {$i < $nfacs } {incr i} {
#     set facname [ gtkwave::getFacName $i ]
#     set indx [ string first "\[1:48\]" $facname  ]
#     if {$indx == -1} {
#     set indx [ string first clk $facname  ]
#   }	
# 
#     if {$indx != -1} {
#       lappend clk48 "$facname"
#   }
# }
# 
#
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

set highLight_insn [list]
lappend highlight_insn "insn"
set num_added [ gtkwave::highlightSignalsFromList $highlight_insn ]
puts "num highlighted: $num_added"
set procFile "../test/filter/filter"
set which_f [ gtkwave::setCurrentTranslateProc $procFile ]
set num_update [ gtkwave::installProcFilter $which_f ]
puts "num updated with proc filter: $num_update"

gtkwave::/Edit/Set_Trace_Max_Hier 0
gtkwave::/Time/Zoom/Zoom_Amount -21.5
# zoom amount - 21.5

#gtkwave::setMarker 128
#gtkwave::setNamedMarker A 400 "Example Named Marker"

#gtkwave::/File/Print_To_File PS {Letter (8.5" x 11")} Full $dumpname.ps
#gtkwave::/File/Quit
