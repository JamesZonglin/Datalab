
/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use // printf for debugging without including
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
	 *        y as 				 1, 0, 1, 0
	 * The result of (~x & y) is 0, 0, 1, 0 - A
	 * The result of (x & ~y) is 0, 1, 0, 0 - B
	 * So now we only have to get the intersection of those two
	 * To get the intersection, simply compute ~(~A & ~B)
	 */
	return ~(~(~x & y) & ~(x & ~y));
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
	 * Also, it could be useful to use a magic number as the identity of
	 * odd value in the range of the number we are allowed to use in this
	 * assignment, which is 0xaa s1, s2, s3 each shift one byte, two bytes,
	 * and three bytes
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
 *   Examples: rotateRight(0x87654321,4) = 0x76543218
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
	int neg_n = ~n + 33;
	int m = ~(~0 << n);
	int mask_1 = ~(m << neg_n);
	int mask_2 = ~mask_1;
	int b0 = (x << neg_n) & mask_2;
	int b1 = (x >> n) & mask_1;
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
	/*
	 * The first assignment of result computes the bit counts group
	 * by group, each group is in two bit size
	 *
	 * For instance, if x = 11010011, then the result of the first
	 * assignment is 10010010, and group 1 has 2 bits, group 2 has
	 * one bit, group 3 has 0 bits and group 4 has 2 bits
	 *
	 * The second assignment computes the bit counts for group in
	 * four bits long, and we compute the bit counts until the group
	 * is 32 bits long
	 *
	 * The implementation make use of magic numbers:
	 * 	0x55555555 for compute group size of 2 bits (010101...)
	 * 	0x33333333 for compute group size of 4 bits (00110011...)
	 * 	0x0f0f0f0f for compute group size of 8 bits (00001111...)
	 * 	0x00ff00ff for compute group size of 16 bits (0000000011111111...)
	 * 	0x0000ffff for compute group size of 32 bits (00000000000000001111111111111111...)
	 *
	 * However it takes too much operators (more than 40) to simply do this:
	 *	x = (x & magic_0) + ((x >> 1) & magic_0);
	 *	x = (x & magic_1) + ((x >> 2) & magic_1);
	 *	x = (x & magic_2) + ((x >> 4) & magic_2);
	 *	x = (x & magic_3) + ((x >> 8) & magic_3);
	 *	x = (x & magic_4) + ((x >> 16) & magic_4);
	 *
	 * To reduce the number of ops, we try to make use of distributivity over '&' if
	 * x + (x >> n) does not overflow:
	 *	x = (x & magic_4) + ((x >> 16) & magic_4);
	 *	x = ((x + (x >> 16)) & magic_4);
	 */
	int magic_0 = 0x55 | 0x55 << 8 | 0x55 << 16 | 0x55 << 24;
	int magic_1 = 0x33 | 0x33 << 8 | 0x33 << 16 | 0x33 << 24;
	int magic_2 = 0xF  | 0xF << 8  | 0xF << 16  | 0xF << 24;
	int magic_3 = 0xFF | 0xFF << 16;
	int magic_4 = 0xFF | 0xFF << 8;

	x = (x & magic_0) + ((x >> 1) & magic_0);
	x = (x & magic_1) + ((x >> 2) & magic_1);
	x = ((x + (x >> 4)) & magic_2);
	x = ((x + (x >> 8)) & magic_3);
	x = ((x + (x >> 16)) & magic_4);

	return x;
}
/* 
 * TMax - return maximum two's complement integer 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmax(void) {
	return ~(0x1 << 31);
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
	int x_sign = (x >> 31) & 1;
	int y_sign = (y >> 31) & 1;
	int xy_sign = ((x+y) >> 31) & 1;

	int diff_overflow = !!(x_sign ^ y_sign);
	int same = !(x_sign ^ xy_sign) & !(y_sign ^ xy_sign);

	return same | diff_overflow;
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
	/*
	 * mask_1: if n=3 then mask_1=0111
	 *         if n=5 then mask_1=011111
	 *
	 * mask_2: just invert mask_1
	 * check_sign: add sign extention bits
	 */
	int mask_1 = ~(~0 << n);
	int mask_2 = ~mask_1;
	int check_sign = ~((x >> 31) & 1) + 1;
	int res_no_sign = x & mask_1;
	int res = res_no_sign | (mask_2 & check_sign & (~res_no_sign+1));
	return res;
}
/*
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0x60000000) = 0x80000000 (saturate to TMin) 		// should the argument be greater than 0x80000000?
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x) {
	/*
	 * if x << 1 changed the sign bit (overflow) then there are two possibilies
	 * in bit representation:
	 * 1. original sign bit is 0 => return sat2TMax
	 * 2. original sign bit is 1 => return sat2TMin
	 */
	int sat2TMin = 1 << 31; 				// 0x80000000
	int sat2TMax = ~sat2TMin; 				// 0x7fffffff
	int ret = x << 1;
	int sign = x >> 31; 					// if sign then fffff..., or 000000..
	int sign_diff = (x ^ ret) >> 31; 		// if sign_diff then fffff..., or 000000...
	ret = ~sign_diff & ret;
	ret |= ~sign & sign_diff & sat2TMax;
	ret |= sign & sign_diff & sat2TMin;
	return ret;
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
	 * x could be greater than y if:
	 * 1. if x >= 0 and y < 0
	 * 		this one is implemented as mask_1
	 * 2. if both x and y is positive and (x - y - 1 >= 0)
	 * 		this one is implemented as mask_2
	 * 		to be noticed, the result of mask_2 is only make sense for the sign bit
	 */
	int x_minus_y_1 = x + ~y;
	int mask_1 = ~x & y;
	int mask_2 = (~(x ^ y)) & ~x_minus_y_1;

	return (mask_1 | mask_2) >> 31 & 1;
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
	 * Add bias, which is (1<<3)-1=7, to multFive before the right shift (division)
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
	 * First of all, we have to find an expression that makes zero so unique
	 * that every other value evaluated by the expression do not have the same
	 * result as the result that zero has
	 * x | (~x + 1) do such magic for us, it has zero evaluated as zero, and
	 * every other value evaluated as non-zero
	 */
	int magic = x | (~x + 1);
	return (magic >> 31) & 1;
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
	/*
	 * It could take more than 10 ops to extract the bit pattern for sign,
	 * exp, and fraction respectively for solving this problem (takes 11 ops)
	 * Therefore, we have to use the range of NaN to solve this problem
	 * Firstly, the definition of NaN is exp=-1, frac!=0, and sign could be
	 * both 0 or 1
	 * We simply mask out the sign bit and make comparison for the NaN
	 */
	unsigned sign_mask = 0x7fffffff;
	unsigned nan = 0x7f800000;
	unsigned result = (uf & sign_mask);

	if (result > nan)
		return uf;
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
	/*
	 * To convert float to int, we have to convert all value with exp
	 * less than 127 as zero, which is an underflow, and convert all value
	 * that has exp > 158 (same as E > 31) to be 0x80000000u
	 *
	 * bias = 2^k-1 - 1 = 127
	 * E for norm = exp - bias
	 * E for denorm = 1 - bias or (exp + 1) - bias
	 */
	int sign_mask = 0x80000000;
	int exp = (uf << 1) >> 24;

	if (exp < 127) { 				// denorm
		return 0x0;
	}
	else { 							// norm
		int E = exp - 127;
		if (exp > 158)
			return 0x80000000u;
		else if (uf & sign_mask) 	// less than 0
			return -(1 << E);
		else
			return (1 << E);
	}
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
	/*
	 * For the normal numbers, we just need to substract the number of exp
	 * However, we have to handle several special cases for this problem:
	 * 1. exp == 0xff, then it's a NaN or special value, we have to return uf
	 * 2. exp == 0, we have to return the result rounding to nearest even
	 * 3. exp == 1, we need to turn exp to 0, and set frac's msb to 1
	 * The toughest problem here is how to round to the nearest even, and we have
	 * tried testing (frac & 0x1), which is the normal way we determine if a number
	 * is an odd, however, if uf is 0x1 we should return 0x0 rather than 0x1.
	 * And then we tried ((frac & 0x3) == 0x3), which works!
	 * If we use a mask that is larger than 0x3, that said, 0x7, then our branch
	 * rejects some frac that is odd.
	 */
	int sign_mask = 0x80000000;
	int frac_mask = 0x7fffff;
	int sign = uf & sign_mask;
	int exp = (uf >> 23) & 0xff;
	int frac = uf & frac_mask; 	 // assume that msb is always 0 after the mask

	if(exp == 0xff) {
		return uf;
	}
	else if (exp > 1) {
		exp -= 1;
		return sign | exp << 23 | frac;
	}
	else {
		if (exp == 1) 			// special case, use 0x00800000 as an example will be much easier to understand
			frac = frac | (1 << 23);
		if ((frac & 0x3) == 0x3)	// round to the nearest even
			frac++;
		frac = frac >> 1;
		return sign | frac;
	}
}