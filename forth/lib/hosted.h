/* 
 * Richard James Howe
 * Howe Forth.
 *
 * Desktop Interface, header
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */
#ifndef hosted_h_header_guard    /* begin header guard for hosted.h */
#define hosted_h_header_guard

#define CALLOC_FAIL(X,RET)\
      if((X)==NULL){\
          fprintf(stderr,"calloc() failed <%s:%d>\n", __FILE__,__LINE__);\
          return (RET);\
      }

void debug_print(fobj_t * fo);
fobj_t *forth_obj_create(mw reg_l, mw dic_l, mw var_l, mw ret_l, mw str_l, FILE *input);
void forth_obj_destroy(fobj_t * fo);

#endif                          /*end header guard for hosted.h */
