/*****************************************************************************\
* H2 CPU program                                                              *
* Richard James Howe                                                          *
* License LGPL                                                                *
\*****************************************************************************/

/* System includes */

#include "sys.inc"
#include "ascii.inc"

/* Variable Addr. */
#define vga_cursor eval("1024")

/*** System Macros ***********************************************************/

/*setup routines go here*/

#define setup \
  vga_init_val \
  o_vgaCtrl \
  !

/* write to the LEDs*/
#define write_LED \
  o_ledS \
  ! 

/* read from the switches*/
#define read_SWITCHES \
  i_switches \
  0 \
  @ \

#define memload \
  0 \
  @

/*****************************************************************************/


/*** program entry point *****************************************************/
main:
 43775 o_8SegLED_0_1 !
 21777 o_8SegLED_2_3 !

 setup

 wait:
  i_ascii_new  @ jumpc wait
  i_ascii_char @ dup o_ledS !

  o_vgaTxtDin !

  call loadcursor
  o_vgaTxtAddr !

  /* increment cursor variable */
  call loadcursor
  1 +
  vga_cursor !

  /* load in cursor, output to actual VGA cursor*/
  call loadcursor
  o_vgaCursor !

  0 o_vgaWrite !

jump wait

/*** Functions ***************************************************************/
loadcursor:
  vga_cursor
  memload
  exit

/*** interrupt service routines **********************************************/
isr isr_clock
isr isr_unused01
isr isr_unused02
  exit
/*****************************************************************************/

/*****************************************************************************\
*** EOF ***********************************************************************
\*****************************************************************************/
