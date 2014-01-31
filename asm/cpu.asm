# Richard James Howe
# h2 test program

$vga_init_val "122"
$vga_ctrl_reg "3 13 << 3 or"

# setup routines go here
%macro setup
  vga_init_val 
  vga_ctrl_reg 
  !
%endmacro

setup
2
begin:
 2 + 
jump begin

