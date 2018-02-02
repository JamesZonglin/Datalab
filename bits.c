/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 *
 * Name: Zonglin Li   UserID: zli80
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* We do support the IEC 559 math functionality, real and complex.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
/* 
 * bitXor - x^y using only ~ and & 
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitXor(int x, int y) {
	/*
	 * Assume x as 				 1, 1, 0, 0
	 * 		  y as 				 1, 0, 1, 0
	 * The result of (~x & y) is 0, 0, 1, 0 - A
	 * The result of (x & ~y) is 0, 1, 0, 0 - B
	 * So now we only have to get the intersection of those two
	 * To get the intersection, simply compute ~(~A & ~B)
	 */
	//return ~(~(~x & y) & ~(x & ~y));

	/*
	 * x^y = (x|y) & (~x | ~y) = ~(~x & ~y) & (~(x & y))
	 */
	return ~(~x & ~y) & (~(x & y));
}
/* 
 * oddBits - return word with all odd-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int oddBits(void) {
	/*
	 * From the description, we know that we can assume int to be 32 bits, therefore,
	 * the number we return should be only 32 bits
	 * Also, it could be useful to use a magic number as the identity of odd value in the range
	 * of the number we are allowed to use in this assignment, which is 0xaa
	 * s1, s2, s3 each shift one byte, two bytes, and three bytes
	 */
	int magic = 0xaa;
	int s1 = 8, s2 = 16, s3 = 24;
	return magic | (magic << s1) | (magic << s2) | (magic << s3);
}
/* 
 * reverseBytes - reverse the bytes of x
 *   Example: reverseBytes(0x01020304) = 0x04030201
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 3
 */
int reverseBytes(int x) {
	/*
	 * x: 	   b3 b2 b1 b0
	 * result: b0 b1 b2 b3
	 * Things we have to do is only move the byte block to the right place
	 * Therefore, we just shift the block to the right and mask with identity,
	 * so that we can get the value of the block. And then we just need to shift
	 * the block back to the right place
	 */
	int s1 = 8, s2 = 16, s3 = 24;
	int identity = 0xff;
	int b0 = x << s3;
	int b1 = ((x >> s1) & identity) << s2;
	int b2 = ((x >> s2) & identity) << s1;
	int b3 = (x >> s3) & identity;
	return b0 | b1 | b2 | b3;
}
/* 
 * rotateRight - Rotate x to the right by n
 *   Can assume that 0 <= n <= 31
 *   Examples: rotateRight(0x87654321,4) = 0x18765432
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 3 
 */
int rotateRight(int x, int n) {
	/*
	 * neg_n = 32 - n
	 * m is the basic of mask, if n = 1 then m = 0x1, n=2 then m = 0x11...
	 * mask_1 is for achieving logical shift, that said, if n=1, mask_1=0x7ffffff
	 * therefore, we can mask out 1's added by the arithmetic shift
	 * b0 is the number that cannot rotate to right anymore, so it has to be at the left side
	 * b1 is just rotate to right
	 */

	// int neg_n = ~n + 33;
	// int m = ~(~0 << n);
	// int mask_1 = ~(m << neg_n);
	// int mask_2 = ~mask_1;
	// int b0 = (x << neg_n) & mask_2;
	// int b1 = (x >> n) & mask_1;
	// return b0 | b1;

	int opposite = 32 - n;
	int b0 = x << opposite;
	int tmp1 = ~(0xFFFFFFFF << n);
	int tmp2 = ~(tmp1 << opposite);
	int b1 = (x >> n) & tmp2;
	return b0 | b1;

}
/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int bitCount(int x) {

	int tmp = 0x1 | 0x1 << 8 | 0x1 << 16 | 0x1 << 24; //tmp = 0x01010101
	int ans = tmp & x;		//test whether the 0th,8th,16th and 24th bits of x are 1 or not
	ans += tmp & (x>>1);	//test whether the 1st,9th,17th and 25th bits of x are 1 or not
	ans += tmp & (x>>2);	//test whether the 2nd,10th,18th and 26th bits of x are 1 or not
	ans += tmp & (x>>3);	//test whether the 3rd,11th,19th and 27th bits of x are 1 or not
	ans += tmp & (x>>4);	//test whether the 4th,12th,20th and 28th bits of x are 1 or not
	ans += tmp & (x>>5);	//test whether the 5th,13th,21st and 29th bits of x are 1 or not
	ans += tmp & (x>>6);	//test whether the 6th,14th,22nd and 30th bits of x are 1 or not
	ans += tmp & (x>>7);	//test whether the 7th,15th,23rd and 31st bits of x are 1 or not
	ans += (ans>>16);		//accumulate high 16 bits and low 16 bits
	ans += (ans>>8);		//accumulate high 8 bits and low 8 bits of the  lower 16 bits of ans
	return ans & 0xff;		//remain the lowest byte of ans and it's the answer
}
/* 
 * TMax - return maximum two's complement integer 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmax(void) {
	return ~(1 << 31);
	//return 0x7FFFFFFF;
}
/* 
 * addOK - Determine if can compute x+y without overflow
 *   Example: addOK(0x80000000,0x80000000) = 0,
 *            addOK(0x80000000,0x70000000) = 1, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int addOK(int x, int y) {
	/*
	 * if the sign bit are different for x and y, then it won't overflow
	 * since they will cancel each other
	 * if x_sign bit and y_sign bit are the same, we know that if sign bit
	 * of x+y being changed, then it overflows
	 */
	// int x_sign = (x >> 31) & 1;
	// int y_sign = (y >> 31) & 1;
	// int xy_sign = ((x+y) >> 31) & 1;

	// int diff_overflow = !!(x_sign ^ y_sign);
	// int same = !(x_sign ^ xy_sign) & !(y_sign ^ xy_sign);

	// return same | diff_overflow;

	int sx = (x >> 31) & 1;
  	int sy = (y >> 31) & 1;
  	int ssum = ((x+y) >> 31) & 1;
  	return !((~(sx ^ sy)) & (sy ^ ssum));
}
/* 
 * rempwr2 - Compute x%(2^n), for 0 <= n <= 30
 *   Negative arguments should yield negative remainders
 *   Examples: rempwr2(15,2) = 3, rempwr2(-35,3) = -3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int rempwr2(int x, int n) {

	//???
 	// int p = (~0) << n;
  //   int ans = x & (~p);
  //   return ans | ((( x & (~ans + 1)) >> 31) & p);  
	return 2;
  

}
/*
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0x60000000) = 0x80000000 (saturate to TMin) ?????????????????????????????
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x) {
	int ans = x << 1;
	int mask_1 = x >> 31;
	int mask_2 = (x ^ ans) >> 31;
	int tmin = 1 << 31;
	return ((~mask_2) & ans) | (mask_2 & (tmin + (~mask_1)));

	// int sat2TMin = 1 << 31; 				// 0x80000000
	// int sat2TMax = ~sat2TMin; 				// 0x7fffffff
	// int ret = x << 1;
	// int sign = x >> 31; 					// if sign then fffff..., or 000000..
	// int sign_diff = (x ^ ret) >> 31; 		// if sign_diff then fffff..., or 000000...
	// ret = ~sign_diff & ret;
	// ret |= ~sign & sign_diff & sat2TMax;
	// ret |= sign & sign_diff & sat2TMin;
	// return ret;
}
/* 
 * isGreater - if x > y  then return 1, else return 0 
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y) {
	/*
	 *  signx      signy        		symx
	 *    0			 0			 1/0 (y < x or y = x)
	 *	  0			 1 			 1/0 (y < x or overflow)
	 * 	  1			 1			 1/0 (y < x or y = x)
	 * 	  1			 0			 0/1 (y > x or overflow)
	 */
	int signx = (x >> 31) & 1;
	int signy = (y >> 31) & 1;
	int symx = ((y + ~x + 1) >> 31) & 1;
	return (symx & 1) ^ ((~(signx^symx)) & (signy^symx));
}
/*
 * multFiveEighths - multiplies by 5/8 rounding toward 0.
 *   Should exactly duplicate effect of C expression (x*5/8),
 *   including overflow behavior.
 *   Examples: multFiveEighths(77) = 48
 *             multFiveEighths(-22) = -13
 *             multFiveEighths(1073741824) = 13421728 (overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 3
 */
int multFiveEighths(int x) {
	/*
	 * If the sign bit of x is zero, then we don't need to deal with the round down
	 * problem, otherwise add (1<<3)-1=7 to multFive before the right shift (division)
	 * The round down problem for two's complement is illustrated in CSAPP p.106
	 */
	int multFive = (x << 2) + x;
	int bias = 7 & (multFive >> 31);
	return (multFive + bias) >> 3;
}
/* 
 * isNonZero - Check whether x is nonzero using
 *              the legal operators except !
 *   Examples: isNonZero(3) = 1, isNonZero(0) = 0
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4 
 */
int isNonZero(int x) {
	/*
	 * x | (~x + 1) produce zero, not-zero respectively when x being zero, not-zero
	 * Also, if x is nozero, the sign bit of temp must be 1
	 */
	int temp = x | (~x + 1);
	return (temp >> 31) & 1;
}
/* 
 * float_abs - Return bit-level equivalent of absolute value of f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_abs(unsigned uf) {
	unsigned mask = 0x7fffffff;
	unsigned NaN = 0x7f800000;
	unsigned result = uf & mask;
	if(result > NaN){
		return uf;
	}
	else
		return result;
}
/* 
 * float_f2i - Return bit-level equivalent of expression (int) f
 *   for floating point argument f.
 *   Argument is passed as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point value.
 *   Anything out of range (including NaN and infinity) should return
 *   0x80000000u.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
int float_f2i(unsigned uf) {
	int exp = (uf >> 23) & 0xff;
	int sign = uf & 0x80000000;
	int frac = uf & 0x007fffff;
	int E = exp - 127;
	if(E > 31 || exp == 255){
		return 0x80000000u;
	}
	else if(E < 0){
		return 0;
	}

	// append a 1 to the front to normalize
  	frac = frac | (1 << 23);

  	// float based on the E
  	if (E > 23) {
    	frac = frac << (E - 23);
  	} 
  	else {
    	frac = frac >> (23 - E);
  	}

  	if(sign){
  		frac = ~frac + 1;
  	}
	return frac;
}
/* 
 * float_half - Return bit-level equivalent of expression 0.5*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_half(unsigned uf) {
	int sign = uf & 0x80000000;
    int exp = (uf >> 23) & 0xff;
    int frac = uf & 0x007fffff;

    if(exp == 0xff)
        return uf;
    else if (exp > 1)
        return sign | --exp << 23 | frac;
    else {
        if (exp == 1) 			//special case, use 0x00800000 as an example will be much easier to understand
            frac = frac | (1 << 23);
        if ((frac & 3) == 3)	//round to even
            frac++;
        frac = frac >> 1;
        return sign | frac;
    }
}
