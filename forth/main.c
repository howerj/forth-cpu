/** Howe Forth
 *
 * @file main.c
 * @brief Wrapper/Top Level.
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */

#include <stdio.h>      /* required by hosted.h and forth.h */
#include <stdlib.h>     /* required by hosted.h */
#include <string.h>     /* strcmp */
#include "lib/forth.h"  /* forth_monitor, fobj_t */
#include "lib/hosted.h" /* forth_obj_create, forth_obj_destroy */

#define MAX_REG 32
#define MAX_DIC (1024*1024)
#define MAX_VAR 8192
#define MAX_RET 8192
#define MAX_STR (1024*1024)

/** versionmsg, used for a primitive version control */
static const char versionmsg[] =
"Howe Forth, version: " __DATE__ " " __TIME__ "\n";

/** The help message! */
static const char helpmsg[] =
"\
Usage: forth [OPTION/FILE]\n\
Run the Howe Forth interpreter.\n\
  -h, --help      Display this help message and exit.\n\
  -V, --version   Display the version number and exit.\n\
\n\
With no OPTION or FILE given Howe Forth will attempt to read the\n\
input file \"forth.4th\", otherwise if a valid file name is given\n\
that will be read in. Once it has it finished it will continue\n\
reading from stdin.\n\
";

/**main, where the magic happens, well is called from.*/
int main(int argc, char *argv[])
{
  FILE *input=NULL;
  mw forth_return;
  fobj_t *fo;

  if(argc > 1){ /**process command line arguments*/
    if((!strcmp("-h",argv[1]))||(!strcmp("--help",argv[1]))){
      fprintf(stdout,"%s\n%s\n",versionmsg,helpmsg);
      return 0;
    }

    if((!strcmp("-V",argv[1]))||(!strcmp("--version",argv[1]))){
      fprintf(stdout,versionmsg);
      return 0;
    }

    /*
    if((!strcmp("-",argv[1]))||(!strcmp("--stdin",argv[1]))){
    }*/
    if(NULL == (input = fopen(argv[1], "r"))){
      fprintf(stderr,"Could not open \"%s\" for reading.\n",argv[1]);
      return 1;
    }
  }

  fo = forth_obj_create(MAX_REG, MAX_DIC, MAX_VAR, MAX_RET, MAX_STR, input);
  CALLOC_FAIL(fo, -1);          /*memory might not be free()'d on error. */
  forth_return = forth_monitor(fo);
  fprintf(stderr, "(returned %X)\n", (unsigned int)forth_return);
  forth_obj_destroy(fo);
  return 0;
}
