/** 
 * @file ccordic.c
 * @brief Test file for cordic algorithms in C 
 *
 * 16 bit fixed point (2.14) CORDIC altered from:
 * http://www.dcs.gla.ac.uk/~jhw/cordic/index.html
 *
 * Original by:    John Williamson
 * Alterations by: Richard James Howe
 *
 * See also:       http://www.jjj.de/fxt/ 
 */

#include <stdint.h>
#include <stdio.h>
#include <math.h> /* for test purpose *only* */

#define M_PI (3.1415926536897932384626f)

void cordic(int16_t angle, int16_t *sine, int16_t *cosine, uint8_t n);

#define CORDIC_1K_M             (0x26DD)
#define HALF_PI_M               (0x6487)
#define SCALING_MULTIPLIER_M    (16384.000000f)
#define CORDIC_ITERATIONS_MAX_M (16u) /**The more iterations we have the greater the precision, upto a point, this is that point.*/

/**
 * Precalculated table from http://www.dcs.gla.ac.uk/~jhw/cordic/index.html
 * circa 2013.
 */
int16_t cordic_table [CORDIC_ITERATIONS_MAX_M] = {
  0x3243, 0x1DAC, 0x0FAD, 0x07F5, 
  0x03FE, 0x01FF, 0x00FF, 0x007F, 
  0x003F, 0x001F, 0x000F, 0x0007, 
  0x0003, 0x0001, 0x0000, 0x0000
};

/** Calculate CORDIC*/
void cordic(int16_t angle, int16_t *sine, int16_t *cosine, uint8_t n){
  uint8_t i; /** loop counter */
  int16_t d,x_c = CORDIC_1K_M, y_c=0, z_c = angle; /** current state */
  int16_t x_n, y_n, z_n;  /**next state*/

  /*
  if((NULL == sine) || (NULL == cosine)){
  }*/

  /**never go above CORDIC_ITERATIONS_MAX_M */
  n = (n > CORDIC_ITERATIONS_MAX_M) ? CORDIC_ITERATIONS_MAX_M: n;

  for(i = 0; i<n; i++){
    /**calculate sign*/
    d = (z_c>=0) ? 0 : -1; /** replaced d = z_c>>15 */

    /**calculate next state*/
    x_n = x_c - (((y_c >> i) ^ d) - d);
    y_n = y_c + (((x_c >> i) ^ d) - d);
    z_n = z_c - ((cordic_table[i] ^ d) - d);

    /**current state becomes next state*/
    x_c = x_n; y_c = y_n; z_c = z_n;
  }
  *sine = y_c; *cosine = x_c;
}

#define TEST_X_VALUES (50u)
int main(void){
  int16_t s, c;
  double p;
  unsigned int i;

  for(i = 0; i < TEST_X_VALUES; i++){
    p = (i / (double) TEST_X_VALUES) * M_PI/2; 
    cordic(p * SCALING_MULTIPLIER_M, &s, &c, CORDIC_ITERATIONS_MAX_M);
    printf("a(%f) s(%f:%f) c(%f:%f) i(%04x) o(%04x %04x)\n",
        p, sin(p), ((double)s/SCALING_MULTIPLIER_M), cos(p), 
        ((double)c/SCALING_MULTIPLIER_M),(int16_t)(p * SCALING_MULTIPLIER_M), 
        s,c
        );
  }

  return 0;
}
