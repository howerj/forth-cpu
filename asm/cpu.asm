###############################################################################
# Richard James Howe                                                          #
# H2 CPU program                                                              #
###############################################################################

#### System includes ##########################################################

# include system varables
%include sys.inc

#### Variable Addr. ###########################################################

$vga_cursor "8000 "

###############################################################################

#### System Macros ############################################################



# setup routines go here
%macro setup
  # Setup Clock
#  clock_init_val
#  o_timerCtrl 
#  store
  # toggle_interrupts

  # Setup VGA
  vga_init_val 
  o_vgaCtrl 
  store
%endmacro

# write to the LEDs
%macro write_LED
  o_ledS
  store
%endmacro

# read from the switches
%macro read_SWITCHES
  i_switches 
  load
%endmacro

%macro memload
  0
  load
%endmacro

###############################################################################



#### program entry point ######################################################
setup
begin:
  255
  memload

  1
  add

  255
  store

  255
  memload
  o_vgaCursor
  store

  255
  memload
  o_vgaTxtDin
  store

  255
  memload
  o_vgaTxtAddr
  store
  
  0
  o_vgaWrite
  store

jump begin

###############################################################################

#### interrupt service routines ###############################################
isr isr_clock
  # read_SWITCHES
  # write_LED
  exit
isr isr_unused01
  exit
isr isr_unused02
  exit
###############################################################################

###############################################################################
### EOF #######################################################################
###############################################################################

