# Richard James Howe
# h2 test program

%macro vga_init_val
  122
%endmacro

# 3 | 3 << 13
%macro vga_ctrl_reg
  24579
%endmacro

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
