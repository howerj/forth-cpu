###############################################################################
# Richard James Howe                                                          #
# H2 CPU program                                                              #
###############################################################################

#### System includes ##########################################################

# include system varables
%include sys.old.inc

#### Variable Addr. ###########################################################

$vga_cursor "1024 "

###############################################################################

#### System Macros ############################################################

# setup routines go here
%macro setup
# Setup Clock
#  clock_init_val
#  o_timerCtrl 
#  !
#  1
#  set_interrupts

  # Setup VGA
  vga_init_val 
  o_vgaCtrl 
  !
%endmacro

# write to the LEDs
%macro write_LED
  o_ledS
  !
%endmacro

# read from the switches
%macro read_SWITCHES
  i_switches 
  0
  @
%endmacro

%macro memload
  0
  @
%endmacro

###############################################################################



#### program entry point ######################################################
setup
begin:

 wait:
  i_ascii_new  @ jumpc wait
  i_ascii_char @ dup o_ledS !

  o_vgaTxtDin !

  call loadcursor
  o_vgaTxtAddr !

  # increment cursor variable
  call loadcursor
  1 +
  vga_cursor !

  # load in cursor, output to actual VGA cursor
  call loadcursor
  o_vgaCursor !

  0 o_vgaWrite !

jump begin

###############################################################################

loadcursor:
  vga_cursor
  memload
  exit

#### interrupt service routines ###############################################
isr isr_clock
#  read_SWITCHES
#  write_LED
  exit
isr isr_unused01
  exit
isr isr_unused02
  exit
###############################################################################

###############################################################################
### EOF #######################################################################
###############################################################################

