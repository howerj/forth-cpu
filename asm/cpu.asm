# Richard James Howe
# h2 test program

$vga_init_val "122"

# When or'ed with a value, moves it into the
# I/O address range.
$io_range "3 13 <<"

$o_vga_ctrl_reg "3 $io_range or"

# Outputs
# 0x6000 - 0x7FFF
$o_7seg         "0  $io_range or"
$o_ledS         "1  $io_range or"
$o_vgaCursor    "2  $io_range or"
$o_vgaCtrl      "3  $io_range or"
$o_vgaTxtAddr   "4  $io_range or"
$o_vgaTxtDin    "5  $io_range or"
$o_vgaWrite     "6  $io_range or"
$o_uartWrite    "7  $io_range or"
$o_uartStbWrite "8  $io_range or"
$o_uartAckDout  "9  $io_range or"
$o_timerCtrl    "10 $io_range or"

# Inputs
# 0x6000 - 0x7FFF
$i_buttons      "0 $io_range or"
$i_switches     "1 $io_range or"
$i_vgaTxtDout   "2 $io_range or"
$i_uartRead     "3 $io_range or"
$i_uartAckWrite "4 $io_range or"
$i_uartStbDout  "5 $io_range or"


# setup routines go here
%macro setup
  vga_init_val 
  o_vgaCtrl
  !
%endmacro

%macro write_LED
  o_ledS
  !
%endmacro

%macro read_SWITCHES
  i_switches 
  @
%endmacro

setup
begin:
  read_SWITCHES
  write_LED
jump begin

