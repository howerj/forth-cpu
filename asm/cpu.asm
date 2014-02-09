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

###############################################################################



#### program entry point ######################################################
setup
begin:
  255
  load 

  1
  add

  255
  swap
  store

  255
  load

  o_vgaTxtAddr
  store

  101
  o_vgaTxtDin
  store


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

