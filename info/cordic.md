# Cordic Example One:

From <http://www.dcs.gla.ac.uk/~jhw/cordic/>

## CORDIC.C

CORDIC generator:

	#include <stdio.h>
	#include <math.h>
	#include <stdint.h>


	//#define M_PI 3.1415926536897932384626
	#define K1 0.6072529350088812561694

	int main(void)
	{
		int i;
		int bits = 16; // number of bits 
		int mul = (1<<(bits-2));


		int n = bits; // number of elements. 
		int c;

		printf("//Cordic in %d bit signed fixed point math\n", bits);
		printf("//Function is valid for arguments in range -pi/2 -- pi/2\n");
		printf("//for values pi/2--pi: value = half_pi-(theta-half_pi) and similarly for values -pi---pi/2\n");
		printf("//\n");
		printf("// 1.0 = %d\n", mul);
		printf("// 1/k = 0.6072529350088812561694\n");
		printf("// pi = 3.1415926536897932384626\n");

		printf("//Constants\n");
		printf("#define cordic_1K 0x%08X\n", (int)(mul*K1));
		printf("#define half_pi 0x%08X\n", (int)(mul*(M_PI/2)));
		printf("#define MUL %f\n", (double)mul);
		printf("#define CORDIC_NTAB %d\n", n);

		printf("int cordic_ctab [] = {");
		for(i=0;i<n;i++) {
			c = (atan(pow(2, -i)) * mul);
			printf("0x%08X, ", c);
		}
		printf("};\n\n");

		//Print the cordic function
		printf("void cordic(int theta, int *s, int *c, int n)\n{\n  int k, d, tx, ty, tz;\n");
		printf("  int x=cordic_1K,y=0,z=theta;\n  n = (n>CORDIC_NTAB) ? CORDIC_NTAB : n;\n");
		printf("  for (k=0; k<n; ++k)\n  {\n    d = z>>%d;\n", (bits-1));
		printf("    //get sign. for other architectures, you might want to use the more portable version\n");
		printf("    //d = z>=0 ? 0 : -1;\n    tx = x - (((y>>k) ^ d) - d);\n    ty = y + (((x>>k) ^ d) - d);\n");
		printf("    tz = z - ((cordic_ctab[k] ^ d) - d);\n    x = tx; y = ty; z = tz;\n  }  \n *c = x; *s = y;\n}\n");
		return 0;   
	}


## GENERATED.C

Modified generated code:

	#include <stdio.h>

	/* See http://www.dcs.gla.ac.uk/~jhw/cordic/ */

	//Cordic in 16 bit signed fixed point math
	//Function is valid for arguments in range -pi/2 -- pi/2
	//for values pi/2--pi: value = half_pi-(theta-half_pi) and similarly for values -pi---pi/2
	//
	// 1.0 = 16384
	// 1/k = 0.6072529350088812561694
	// pi = 3.1415926536897932384626
	//Constants
	#define cordic_1K 0x000026DD
	#define half_pi 0x00006487
	#define MUL 16384.000000
	#define CORDIC_NTAB 16
	int cordic_ctab [] = {0x00003243, 0x00001DAC, 0x00000FAD, 0x000007F5, 0x000003FE, 0x000001FF, 0x000000FF, 0x0000007F, 0x0000003F, 0x0000001F, 0x0000000F, 0x00000007, 0x00000003, 0x00000001, 0x00000000, 0x00000000, };

	void cordic(int theta, int *s, int *c, int n)
	{
	  int k, d, tx, ty, tz;
	  int x=cordic_1K,y=0,z=theta;
	  n = (n>CORDIC_NTAB) ? CORDIC_NTAB : n;
	  for (k=0; k<n; ++k)
	  {
	    d = z > =0 ? 0 : -1;
	    tx = x - (((y>>k) ^ d) - d);
	    ty = y + (((x>>k) ^ d) - d);
	    tz = z - ((cordic_ctab[k] ^ d) - d);
	    x = tx; y = ty; z = tz;
	  }  
	 *c = x; *s = y;
	}

	#define INIT ((3.14592 /2.0) * 16384)
	int main(void)
	{
		int theta = 0, s = 0, c = 0, n = 0;
		for(theta = INIT; theta > -INIT; theta -= (INIT/20.0)) {
			cordic(theta, &s, &c, CORDIC_NTAB);
			printf("%d, %d, %d\n", theta, s, c);
		}
	}


# Microcontrollers & CORDIC Methods

From: <http://www.drdobbs.com/microcontrollers-cordic-methods/184404244?pgno=2>

By Michael Pascale, September 01, 2000

    cordic.txt
    cordic.zip

CORDIC algorithms are efficient in terms of both computation time and
hardware resources -- and in most microcontroller systems, these resources
are normally a premium.

Elementary functions for embedded systems

Michael is a senior electrical engineer at Basler Electric and part-time
lecturer in the Department of Electrical and Computer Engineering at
Southern Illinois University at Edwardsville. He can be contacted at
mikepashea@basler.com or mpashea@ ee.siue.edu.

Many times in designing software for microcontroller-based systems,
you need to make calculations that involve elementary functions such as
sin(x), cos(x), or log10(x). For example, many temperature sensors are
logarithmic in nature; that is, the sensor output voltage may increase by
x volts each time the temperature doubles. In this case converting the
sensor voltage to a linear temperature scale requires the calculation
of 2x.

Calculation of an elementary function is often times done by using a
look-up table. While look-up tables are by far the fastest way to make
the computation, the precision of the result is directly related to the
size of the look-up table. High-precision look-up tables require large
amounts of nonvolatile memory to store the table. If the table size is
reduced to save memory, precision is also reduced.

Power series may also be used to calculate these same functions without
using look-up tables, however, these calculations have the disadvantage
of being slow to converge to a desired precision. In effect, the look-up
table size is being traded at the expense of computation time.

CORDIC methods of computation represent a compromise between these
two methods. The CORDIC technique uses a one-bit-at-a-time approach
to make computations to an arbitrary precision. In the process,
relatively small look-up tables are used for constants necessary for
the algorithm. Typically, these tables require only one to two entries
per bit of precision. CORDIC algorithms also use only right shifts and
additions, minimizing the computation time.

## Fundamentals of CORDIC Algorithms

All CORDIC algorithms are based on the fact that any number may
be represented by an appropriate alternating series. For example,
an approximate value for e can be represented as: e3-0.3+0.02-0.002+
0.0003=2.7183. In this case, each digit gives an additional power of 10
resolution to the approximation of the value for e. Also, if the series
is truncated to a certain number of terms, the resulting value will be
the same as the value obtained by rounding the true value of e to that
number of digits. In general, the series obtained for a value by this
method does not always alternate regularly. The series for p, for example,
is p3+0.1+0.04+0.002- 0.0004-0.00001=3.14159. The series for e is also
irregular if the expansion is continued for a few additional terms.

The CORDIC technique uses a similar method of computation. A value to be
computed, such as sin(x) or log10(x), is considered to be a truncated
series in the format in Figure 1. In this case, the values for ai are
either 0 or 1 and represent bits in the binary representation of z. The
value for z is determined 1 bit at a time by looking at the previously
calculated value for z, which is correct to i-1 bits. If this estimate
of z is too low, you correct the current estimate by adding a correction
factor, obtained from a look-up table, to the current value of z. If the
current estimate of z is too high, you subtract a correction factor,
also from the look-up table. Depending on whether you add or subtract
from the current value of z, the ith bit is set to the correct value of
0 or 1. The less significant bits from i+1 to B may change during this
process because the estimate for z is only accurate to i bits.

Because of the trigonometric relationship between the sin(x) and cos(x)
functions, it is often possible to calculate both of these values
simultaneously. If the cos(x) is considered as a projection onto
the x-axis and sin(x) as a projection onto the y-axis, the iteration
process amounts to the rotation of an initial vector. It is from this
vector rotation that the CORDIC algorithm derives its name -- COordinate
Rotation DIgital Computer.

## Algorithms for Multiplication and Division

A CORDIC algorithm for multiplication can be derived using a series
representation for x, as in Figure 2. From this, z is composed of shifted
versions of y. The unknown value for z, may be found by driving x to zero
1 bit at a time. If the ith bit of x is nonzero, yi is right shifted by
i bits and added to the current value of z. The ith bit is then removed
from x by subtracting 2-i from x. If x is negative, the ith bit in the
twos complement format would be removed by adding 2-i. In either case,
when x has been driven to zero all bits have been examined and z contains
the signed product of x and y correct to B bits.

This algorithm is similar to the standard shift and add multiplication
algorithm except for two important features.

Arithmetic right shifts are used instead of left shifts, allowing
signed numbers to be used.  Computing the product to B bits with
the CORDIC algorithm is equivalent to rounding the result of the
standard algorithm to the most significant B bits.

Listing One is the final multiplication algorithm. This calculation
assumes that both x and y are fractional within the domain-1 to 1. The
algorithm is valid for other domains as long as the decimal point is
allowed to float. With a few extensions, this algorithm would work well
with floating-point data.

A CORDIC division algorithm is based on rewriting the equation z=x/y
into the form x-y\*z=0. If z is expanded into its series representation,
the second version of the equation takes the form in Figure 3(a), which,
after some manipulation, yields Figure 3(b). This final form of the
equation shows that the quotient z may be estimated 1 bit at a time
by driving x to zero using right-shifted versions of y. If the current
residual is positive, the ith bit in z is set. Likewise, if the residual
is negative the ith bit in z is cleared; see Listing Two.

The convergence of this division algorithm is trickier than the
multiplication algorithm. While x may be either positive or negative,
the value for y is assumed to be positive. As a result, the division
algorithm is only valid in two quadrants. Also, if the initial value for
y is less than the initial value for x it will be impossible to drive
the residual to zero. This means that the initial y value must always
be greater than x, resulting in a range of 0&lt;z&lt;1. The algorithm may be
modified as in Listing Three for four quadrant division with -1&lt;z&lt;1.

As with all division algorithms, the case where y is zero should be
trapped as an exception. Once again, a few extensions would allow this
algorithm to work well with floating-point data.

## Algorithms for Log10(x) and 10x

To calculate the base 10 logarithm of a value x, it is convenient
to use the identity in Figure 4(a). If the bi are chosen such that
x\*b1\*b2\*b3...\*bB=1, the left side reduces to log10(1), which is 0. With
these choices for bi, you are left with the equation in Figure 4(b)
for log10(x). Since quantities for log10(bi) may be stored in a look-up
table, the base 10 logarithm of x may be calculated by summing selected
entries from the table.

The trick now is to choose the correct bi such that you drive the product
of x and all of the bi to 1. This may be accomplished by examining
the current product. If the current product is less than 1, you choose
coefficient bi such that bi is greater than 1. On the other hand, if
the current product is greater than 1 the coefficient should be chosen
such that its value is less than 1. An additional constraint is that
the bi should be chosen such that multiplication by any of the bi is
accomplished by a shift and add operation. Two coefficients that have
the desired properties are: bi=1+2-i if x\*b1\*b2...bi-1 &lt;1 and bi=1-2-i
if x\*b1\*b2...bi-1&lt;1. In choosing these values for the bi, the limit, as
i approaches infinity of the product of x and the bis, will be 1 as long
as x is in the domain (see Figure 4(c)). This represents the domain of
convergence for this algorithm, which may be calculated as approximately
0.4194&lt;x&lt;3.4627. If you want to calculate logarithms outside of this
domain, either the input must be prescaled or the range of the i values
must be changed. The final algorithm becomes Listing Four.

To calculate the inverse of this algorithm, or 10x, you only modify the
existing algorithm such that x is driven to zero while z is multiplied
by the successive coefficients, bi. This follows from the fact that if
z=10x then z=bi\*10(x- log10(bi)). As the exponent is driven to zero,
z is seen to approach the product of all the successive coefficients.

	B

	'bi

	i=1

Listing Five is the final algorithm. The domain of convergence for this
algorithm is determined by the possible values for which x can be driven
to zero. By inspection of the algorithm this is determined to be Figure
4(d) or x is limited to the domain -0.5393&lt; x&lt;0.3772. As in the previous
algorithm, the domain may be extended by scaling the initial value of z by
(1+2i) or (1-2i).

The Circular Functions sin(x) and cos(x)

The rotation matrix in Figure 5(a) rotates a vector

	[ x0 ]
	[ y0 ]

counterclockwise by a radians in two-dimensional space. If this rotation
matrix is applied to the initial vector

	[ 1 ]
	[ 0 ]

the result will be a vector with coordinates of

	[ cos a ]
	[ sin a ]

It is easily seen that the CORDIC method could be applied to calculate
the functions sin(x) and cos(x) by applying successive rotations to the
initial vector

	[ 1 ]
	[ 0 ]

and gradually driving the angle a to zero.

However, a problem arises when an attempt is made to set up the rotation
matrix such that all rotations are accomplished by right shifts. If ai
is chosen such that cos(ai)=2-i, the sin(ai) is not necessarily a power
of 2. It is not possible to choose the successive angle rotations, ai,
such that both the cos(ai) and sin(ai) amount to right shifts.

In working around this problem, you can modify the rotation matrix
by bringing a cos(a) term out of the matrix; see Figure 5(b). Now
the rotation angles ai may be chosen such that tan(ai)=2-i or rather
ai=tan-1(2-i). The result is the final incremental rotation matrix in
Figure 5(c), where ai=tan-1(2-i). With these choices for the ai, rotation
is accomplished using only right shifts. If the cos(ai) term is neglected
to avoid the multiplication operations, the length of the initial vector
is increased each time it is rotated by using right shifts only. This
increase may be compensated for by decreasing the length of the vector
prior to rotation. Because the algorithm uses B successive rotations,
all rotations may be compensated for initially using one collective
length correction factor, C. The value of C is found by grouping all of
the ai terms together as in Figure 5(d).

For B=16 bits, C may be calculated as approximately 0.6072. Listing Six
presents the final algorithms, x and y represent vector coordinates,
while z is now the angle register. It can be determined that the previous
two algorithms will converge as long as Figure 5(e) or -1.7433&lt;z&lt;1.7433.

Since the domain of convergence includes both the first and fourth
quadrants, the algorithms will converge for any z such that -p/2&lt;z&lt;p/2.

## Mapping CORDIC Algorithms to Microcontrollers

The previously discussed algorithms show that CORDIC-based computation
methods require minimal hardware features to implement:

    Three registers of length B bits.
    One, two, or three Adders/Subtractors.

    Several small ROM-based look-up tables.

    One, two, or three shift registers.

When implementing CORDIC algorithms on microcontrollers, the shift
registers will have the greatest effect on the overall throughput of
the system. Multiplication by 2-i requires that the shift register be
capable of performing a right shift by i bits. Most microcontrollers are
only capable of right shifting by 1 bit at a time. Shifting by i bits
requires a software loop to repeat this task i times, greatly increasing
the computation time. The 8051, 6805, and 68HC11 are typical examples
of microcontrollers that require software loops to implement the shifter.

Other microcontrollers, such as the 68HC332, as well as most DSPs, have
a feature known as "barrel shifters," which right shifts by i bits in one
operation. Typically the shift is also accomplished in one clock cycle.

Another possibility for implementing a barrel shifter is to use a multiply
instruction that has been optimized for speed. An example of this is the
68HC12, which has a 16×16 bit signed multiply, and EMULS that produces
a 32-bit result in three clock cycles. A right shift by i bits could
be accomplished by multiplying by 216-i and discarding the lower 16
bits of the result. One disadvantage of this scheme is that the data is
restricted to 16 bits. Other word lengths would require additional cycles.

Once the processor and shift register style is chosen, the next choice
to be made involves the data format. Since Standard C does not provide
a fixed-point data type, you have a lot of freedom in choosing the
format of the data. It is a good idea, however, to choose a format that
fits into 16- or 32-bit words. Even though most CORDIC routines are
written in assembly language for speed, 16- or 32-bit words allow data
to be passed as either int or long int data types within higher level C
subroutines. The format presented here uses a 16-bit format with 4 bits to
the left of the decimal point and 12 fractional bits to the right, which
is often referred to as "4.12 format." This allows constants such as p,
e, and --2 to be easily represented without a moving decimal point. The
12 bits of fractional data amount to approximately 3.5 digits of decimal
accuracy. The range of this format is calculated as -8&lt; x&lt;7.9997.

The constants used are found by multiplying by 212 (4096), rounding,
and converting to hexadecimal. Take the constant e, for example:
4096\*e=11134.0811134= 0x267e. All of the data tables necessary for CORDIC
computing may be built up this way using a calculator.

Finally, with the data format and constant tables established, coding
of the algorithms proceeds in a straightforward manner. Available
electronically (see "Resource Center," page 5) are source-code
implementations of CORDIC algorithms for the 8051, 68HCll, and
68332 microcontrollers. This code was assembled with the Intel MCS-51
Macro-Assembler and Motorola Freeware Assemblers and tested on hardware
development systems.

## Conclusion

CORDIC algorithms have been around for some time. For instance, Jack
Volder described them in "The CORDIC Trigonometric Computing" (IRE
Transactions Electronic Computers, September 1959). The reasons for using
CORDIC algorithms have not changed. The algorithms are efficient in terms
of both computation time and hardware resources. In most microcontroller
systems, especially those performing control functions, these resources
are normally already at a premium. Using CORDIC algorithms may allow
a single chip solution where algorithms using the look-up table method
may require a large ROM size or where power series calculations require
a separate coprocessor because of the computation time required.

The algorithms presented have been selected to represent a small core of
functions commonly required in microcontroller systems, which could be
discussed in detail. For each algorithm in this core, three areas have
been covered: theory of operation, determining the domain of convergence
for the algorithm, and finally, implementation of the algorithm on a
typical microcontroller. Using these selected algorithms as a starting
point, you can develop libraries containing many similar elementary
functions. Among those possible with only minor modifications to the
algorithms presented are: lnx, ex, tan-1x, --x2+y2, and ejq. Among the
references, Jarvis gives an excellent table of the functions possible
using CORDIC routines.

## Pseudo C

	multiply(x,y){         
	   for (i=1; i=<B; i++){
	      if (x > 0)
		 x = x - 2^(-i)
		 z = z + y*2^(-i)
	      else  
		 x = x + 2^(-i)
		 z = z - y*2^(-i)
	   }
	   return(z)
	}

	divide(x,y){
	   for (i=1; i=<B; i++){
	      if (x > 0)
		 x = x - y*2^(-i);
		 z = z + 2^(-i);
	      else  
		 x = x + y*2^(-i);
		 z = z - 2^(-i);
	   }
	   return(z)
	}

	divide_4q(x,y){
	   for (i=1; i=<B; i++){
	      if (x > 0)
		if (y > 0)
		   x = x - y*2^(-i);
		   z = z + 2^(-i);
		else
		   x = x + y*2^(-i);
		   z = z - 2^(-i);
	      else          
		 if (y > 0)
		    x = x + y*2^(-i);
		    z = z - 2^(-i);
		 else
		    x = x - y*2^(-i);
		    z = z + 2^(-i);
	   }
	   return(z)
	}

	log10(x){
	   z = 0;
	   for ( i=1;i=<B;i++ ){
	      if (x > 1)
		 x = x - x*2^(-i);
		 z = z - log10(1-2^(-i));
	       else
		 x = x + x*2^(-i);
		 z = z - log10(1+2^(-i));
	   }
	   return(z)
	}

	10_to_power(x){
	   z = 1;
	   for ( i=1;i=<B; i++ ){
	      if (x > 0)
		 x = x - log10(1+2^(-i));
		 z = z + z*2^(-i);
	      else
		 x = x - log10(1-2^(-i));
		 z = z - z*2^(-i);
	   }
	   return(z)
	}

	sin(z){  
	   x = 0.6072;
	   y = 0;       
	   for (i=0; i=<B; i++){
	      if (z > 0)
		 x = x - y*2^(-i)
		 y = y + x*2^(-i)
		 z = z - arctan(2^(-i))
	      else  
		 x = x + y*2^(-i)
		 y = y - x*2^(-i)
		 z = z + arctan(2^(-i))
	   }
	   return(y)
	}

	cos(z){  
	   x = 0.6072;
	   y = 0;
	   for (i=0; i=<B; i++){
	      if (z > 0)
		 x = x - y*2^(-i)
		 y = y + x*2^(-i)
		 z = z - arctan(2^(-i))
	      else
		 x = x + y*2^(-i)
		 y = y - x*2^(-i)
		 z = z + arctan(2^(-i))
	   }
	   return(x)
	}

-- END --

