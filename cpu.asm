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
  ! \
  drop

/* write to the LEDs*/
#define write_LED \
  o_ledS \
  ! \
  drop

/* read from the switches*/
#define read_SWITCHES \
  i_switches \
  @

/*****************************************************************************/


/*** program entry point *****************************************************/
main:
 /* setup

 43775 o_8SegLED_0_1 ! drop
 21777 o_8SegLED_2_3 ! drop
 */

 wait:

  /* increment cursor variable */
  /* call loadcursor
  1 +
  call savecursor */
  1024 @ 1 + 1024 ! drop
  jump wait

  read_SWITCHES write_LED
  i_ascii_new  @ jumpc wait

  call loadcursor
  o_vgaTxtAddr ! drop

  i_ascii_char @ 
  o_vgaTxtDin ! drop

  0 o_vgaWrite ! drop 

  /* increment cursor variable */
  call loadcursor
  1 +
  call savecursor

  /* load in cursor, output to actual VGA cursor*/
  call loadcursor
  o_vgaCursor ! drop


jump wait

/*** Functions ***************************************************************/
loadcursor:
  vga_cursor
  @
  exit

savecursor:
  vga_cursor ! drop
  exit

/*** interrupt service routines **********************************************/
isr isr_clock
isr isr_uart_ack 
isr isr_uart_stb
isr isr_unused03
isr isr_unused04
isr isr_unused05
isr isr_unused06
  exit
/*****************************************************************************/

/*****************************************************************************\
*** EOF ***********************************************************************
\*****************************************************************************/
