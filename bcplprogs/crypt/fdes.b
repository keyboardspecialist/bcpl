/* This is an experimental implementation of the DES Algorithm

   Implemented in BCPL by Martin Richards  (c) June 2001

   Based on the description of DES by J. Orlin Grabbe in
   http://www.zolatimes.com/V2.28/DES.htm and the DES Standard in
   http://csrc.nist.gov/publications/fips/fips46-3/fips43-3.pdf
*/

GET "libhdr"

GLOBAL {
  count:ug
  // The key schedule
  kl1; kl2;  kl3;  kl4;  kl5;  kl6;  kl7;  kl8
  kl9; kl10; kl11; kl12; kl13; kl14; kl15; kl16
  kr1; kr2;  kr3;  kr4;  kr5;  kr6;  kr7;  kr8
  kr9; kr10; kr11; kr12; kr13; kr14; kr15; kr16
}

LET start() = VALOF
{ LET keyl, keyr  = 0, 0
  LET mess = VEC 1
  LET code = VEC 1
  LET res  = VEC 1
//selectoutput(findoutput("res"))
  mess!0, mess!1 := #x01234567, #x89ABCDEF  // The message
  keyl,  keyr    := #x13345779, #x9BBCDFF1  // The key

  writef("*nKey:  %bW %bW*n", keyl, keyr)
  setkeys(keyl, keyr)  // First set the key schedule
  newline()
  FOR i = 0 TO 15 DO writef("K%i2: %bO %bO*n", i+1, (@kl1)!i, (@kr1)!i)

  writef("*nEncryption computation*n*n")
  writef("M:    %bW %bW*n", mess!0, mess!1)
  des   (mess!0, mess!1, code)
  writef("C:    %bW %bW*n", code!0, code!1)
  writef("encode: %x8%x8 with key: %x8%x8 => %x8%x8*n",
                mess!0, mess!1, keyl, keyr, code!0, code!1)
  writef("*nDecryption computation*n*n")
  writef("C:    %bW %bW*n", code!0, code!1)
  invdes(code!0, code!1, res)
  writef("M:    %bW %bW*n", res!0, res!1)
  writef("decode: %x8%x8 with key: %x8%x8 => %x8%x8*n",
                code!0, code!1, keyl, keyr, res!0, res!1)
//endwrite()
  RESULTIS 0
}

AND setkeys(keyl, keyr) BE
{ kl1 := getbits2(24, keyl, keyr, TABLE
                  10,51,34,60,49,17,33,57, 2, 9,19,42,
                   3,35,26,25,44,58,59, 1,36,27,18,41)
  kr1 := getbits2(24, keyl, keyr, TABLE
                  22,28,39,54,37, 4,47,30, 5,53,23,29,
                  61,21,38,63,15,20,45,14,13,62,55,31)
  kl2 := getbits2(24, keyl, keyr, TABLE
                   2,43,26,52,41, 9,25,49,59, 1,11,34,
                  60,27,18,17,36,50,51,58,57,19,10,33)
  kr2 := getbits2(24, keyl, keyr, TABLE
                  14,20,31,46,29,63,39,22,28,45,15,21,
                  53,13,30,55, 7,12,37, 6, 5,54,47,23)
  kl3 := getbits2(24, keyl, keyr, TABLE
                  51,27,10,36,25,58, 9,33,43,50,60,18,
                  44,11, 2, 1,49,34,35,42,41, 3,59,17)
  kr3 := getbits2(24, keyl, keyr, TABLE
                  61, 4,15,30,13,47,23, 6,12,29,62, 5,
                  37,28,14,39,54,63,21,53,20,38,31, 7)
  kl4 := getbits2(24, keyl, keyr, TABLE
                  35,11,59,49, 9,42,58,17,27,34,44, 2,
                  57,60,51,50,33,18,19,26,25,52,43, 1)
  kr4 := getbits2(24, keyl, keyr, TABLE
                  45,55,62,14,28,31, 7,53,63,13,46,20,
                  21,12,61,23,38,47, 5,37, 4,22,15,54)
  kl5 := getbits2(24, keyl, keyr, TABLE
                  19,60,43,33,58,26,42, 1,11,18,57,51,
                  41,44,35,34,17, 2, 3,10, 9,36,27,50)
  kr5 := getbits2(24, keyl, keyr, TABLE
                  29,39,46,61,12,15,54,37,47,28,30, 4,
                   5,63,45, 7,22,31,20,21,55, 6,62,38)
  kl6 := getbits2(24, keyl, keyr, TABLE
                   3,44,27,17,42,10,26,50,60, 2,41,35,
                  25,57,19,18, 1,51,52,59,58,49,11,34)
  kr6 := getbits2(24, keyl, keyr, TABLE
                  13,23,30,45,63,62,38,21,31,12,14,55,
                  20,47,29,54, 6,15, 4, 5,39,53,46,22)
  kl7 := getbits2(24, keyl, keyr, TABLE
                  52,57,11, 1,26,59,10,34,44,51,25,19,
                   9,41, 3, 2,50,35,36,43,42,33,60,18)
  kr7 := getbits2(24, keyl, keyr, TABLE
                  28, 7,14,29,47,46,22, 5,15,63,61,39,
                   4,31,13,38,53,62,55,20,23,37,30, 6)
  kl8 := getbits2(24, keyl, keyr, TABLE
                  36,41,60,50,10,43,59,18,57,35, 9, 3,
                  58,25,52,51,34,19,49,27,26,17,44, 2)
  kr8 := getbits2(24, keyl, keyr, TABLE
                  12,54,61,13,31,30, 6,20,62,47,45,23,
                  55,15,28,22,37,46,39, 4, 7,21,14,53)
  kl9 := getbits2(24, keyl, keyr, TABLE
                  57,33,52,42, 2,35,51,10,49,27, 1,60,
                  50,17,44,43,26,11,41,19,18, 9,36,59)
  kr9 := getbits2(24, keyl, keyr, TABLE
                   4,46,53, 5,23,22,61,12,54,39,37,15,
                  47, 7,20,14,29,38,31,63,62,13, 6,45)
  kl10 := getbits2(24, keyl, keyr, TABLE
                   41,17,36,26,51,19,35,59,33,11,50,44,
                   34, 1,57,27,10,60,25, 3, 2,58,49,43)
  kr10 := getbits2(24, keyl, keyr, TABLE
                   55,30,37,20, 7, 6,45,63,38,23,21,62,
                   31,54, 4,61,13,22,15,47,46,28,53,29)
  kl11 := getbits2(24, keyl, keyr, TABLE
                   25, 1,49,10,35, 3,19,43,17,60,34,57,
                   18,50,41,11,59,44, 9,52,51,42,33,27)
  kr11 := getbits2(24, keyl, keyr, TABLE
                   39,14,21, 4,54,53,29,47,22, 7, 5,46,
                   15,38,55,45,28, 6,62,31,30,12,37,13)
  kl12 := getbits2(24, keyl, keyr, TABLE
                    9,50,33,59,19,52, 3,27, 1,44,18,41,
                    2,34,25,60,43,57,58,36,35,26,17,11)
  kr12 := getbits2(24, keyl, keyr, TABLE
                   23,61, 5,55,38,37,13,31, 6,54,20,30,
                   62,22,39,29,12,53,46,15,14,63,21,28)
  kl13 := getbits2(24, keyl, keyr, TABLE
                   58,34,17,43, 3,36,52,11,50,57, 2,25,
                   51,18, 9,44,27,41,42,49,19,10, 1,60)
  kr13 := getbits2(24, keyl, keyr, TABLE
                    7,45,20,39,22,21,28,15,53,38, 4,14,
                   46, 6,23,13,63,37,30,62,61,47, 5,12)
  kl14 := getbits2(24, keyl, keyr, TABLE
                   42,18, 1,27,52,49,36,60,34,41,51, 9,
                   35, 2,58,57,11,25,26,33, 3,59,50,44)
  kr14 := getbits2(24, keyl, keyr, TABLE
                   54,29, 4,23, 6, 5,12,62,37,22,55,61,
                   30,53, 7,28,47,21,14,46,45,31,20,63)
  kl15 := getbits2(24, keyl, keyr, TABLE
                   26, 2,50,11,36,33,49,44,18,25,35,58,
                   19,51,42,41,60, 9,10,17,52,43,34,57)
  kr15 := getbits2(24, keyl, keyr, TABLE
                   38,13,55, 7,53,20,63,46,21, 6,39,45,
                   14,37,54,12,31, 5,61,30,29,15, 4,47)
  kl16 := getbits2(24, keyl, keyr, TABLE
                   18,59,42, 3,57,25,41,36,10,17,27,50,
                   11,43,34,33,52, 1, 2, 9,44,35,26,49)
  kr16 := getbits2(24, keyl, keyr, TABLE
                   30, 5,47,62,45,12,55,38,13,61,31,37,
                    6,29,46, 4,23,28,53,22,21, 7,63,39)
}

AND des(f0, f1, res, x, y) BE
{ ip(f0, f1, @x) // x and y are consecutive locations
  x := x NEQV f(y, kl1,  kr1);  y := y NEQV f(x, kl2,  kr2)
  x := x NEQV f(y, kl3,  kr3);  y := y NEQV f(x, kl4,  kr4)
  x := x NEQV f(y, kl5,  kr5);  y := y NEQV f(x, kl6,  kr6)
  x := x NEQV f(y, kl7,  kr7);  y := y NEQV f(x, kl8,  kr8)
  x := x NEQV f(y, kl9,  kr9);  y := y NEQV f(x, kl10, kr10)
  x := x NEQV f(y, kl11, kr11); y := y NEQV f(x, kl12, kr12)
  x := x NEQV f(y, kl13, kr13); y := y NEQV f(x, kl14, kr14)
  x := x NEQV f(y, kl15, kr15); y := y NEQV f(x, kl16, kr16)
  invip(y, x, res)
}

AND invdes(f0, f1, res, x, y) BE
{ ip(f0, f1, @x) // x and y are consecutive locations
  x := x NEQV f(y, kl16, kr16); y := y NEQV f(x, kl15, kr15)
  x := x NEQV f(y, kl14, kr14); y := y NEQV f(x, kl13, kr13)
  x := x NEQV f(y, kl12, kr12); y := y NEQV f(x, kl11, kr11)
  x := x NEQV f(y, kl10, kr10); y := y NEQV f(x, kl9,  kr9)
  x := x NEQV f(y, kl8,  kr8);  y := y NEQV f(x, kl7,  kr7)
  x := x NEQV f(y, kl6,  kr6);  y := y NEQV f(x, kl5,  kr5)
  x := x NEQV f(y, kl4,  kr4);  y := y NEQV f(x, kl3,  kr3)
  x := x NEQV f(y, kl2,  kr2);  y := y NEQV f(x, kl1,  kr1)
  invip(y, x, res)
}

AND ip(m0, m1, res) BE
{ res!0 := getbits2(32, m0, m1, TABLE
                    58,50,42,34,26,18,10, 2,60,52,44,36,28,20,12, 4,
                    62,54,46,38,30,22,14, 6,64,56,48,40,32,24,16, 8)
  res!1 := getbits2(32, m0, m1, TABLE
                    57,49,41,33,25,17, 9, 1,59,51,43,35,27,19,11, 3,
                    61,53,45,37,29,21,13, 5,63,55,47,39,31,23,15, 7)
}

AND invip(m0, m1, res) BE
{ res!0 := getbits2(32, m0, m1, TABLE
                    40, 8,48,16,56,24,64,32,39, 7,47,15,55,23,63,31,
                    38, 6,46,14,54,22,62,30,37, 5,45,13,53,21,61,29)
  res!1 := getbits2(32, m0, m1, TABLE
                    36, 4,44,12,52,20,60,28,35, 3,43,11,51,19,59,27,
                    34, 2,42,10,50,18,58,26,33, 1,41, 9,49,17,57,25)
}

AND f(r, kl, kr) = VALOF
{ LET res =       s1((r<<5  | r>>27 NEQV kl>>18) & 63)
  res := res<<4 | s2((        r>>23 NEQV kl>>12) & 63)
  res := res<<4 | s3((        r>>19 NEQV kl>> 6) & 63)
  res := res<<4 | s4((        r>>15 NEQV kl    ) & 63)
  res := res<<4 | s5((        r>>11 NEQV kr>>18) & 63)
  res := res<<4 | s6((        r>> 7 NEQV kr>>12) & 63)
  res := res<<4 | s7((        r>> 3 NEQV kr>> 6) & 63)
  res := res<<4 | s8((r>>31 | r<< 1 NEQV kr    ) & 63)

  RESULTIS getbits(32, res,
                   TABLE 16, 7,20,21, 29,12,28,17, 1,15,23,26, 5,18,31,10,
                          2, 8,24,14,32,27, 3, 9,19,13,30, 6,22,11, 4, 25)
}

AND s1(b) = b!TABLE 14, 0, 4,15,13, 7, 1, 4, 2,14,15, 2,11,13, 8, 1,
                     3,10,10, 6, 6,12,12,11, 5, 9, 9, 5, 0, 3, 7, 8,
                     4,15, 1,12,14, 8, 8, 2,13, 4, 6, 9, 2, 1,11, 7,
                    15, 5,12,11, 9, 3, 7,14, 3,10,10, 0, 5, 6, 0,13
AND s2(b) = b!TABLE 15, 3, 1,13, 8, 4,14, 7, 6,15,11, 2, 3, 8, 4,14,
                     9,12, 7, 0, 2, 1,13,10,12, 6, 0, 9, 5,11,10, 5,
                     0,13,14, 8, 7,10,11, 1,10, 3, 4,15,13, 4, 1, 2,
                     5,11, 8, 6,12, 7, 6,12, 9, 0, 3, 5, 2,14,15, 9
AND s3(b) = b!TABLE 10,13, 0, 7, 9, 0,14, 9, 6, 3, 3, 4,15, 6, 5,10,
                     1, 2,13, 8,12, 5, 7,14,11,12, 4,11, 2,15, 8, 1,
                    13, 1, 6,10, 4,13, 9, 0, 8, 6,15, 9, 3, 8, 0, 7,
                    11, 4, 1,15, 2,14,12, 3, 5,11,10, 5,14, 2, 7,12
AND s4(b) = b!TABLE  7,13,13, 8,14,11, 3, 5, 0, 6, 6,15, 9, 0,10, 3,
                     1, 4, 2, 7, 8, 2, 5,12,11, 1,12,10, 4,14,15, 9,
                    10, 3, 6,15, 9, 0, 0, 6,12,10,11, 1, 7,13,13, 8,
                    15, 9, 1, 4, 3, 5,14,11, 5,12, 2, 7, 8, 2, 4,14
AND s5(b) = b!TABLE  2,14,12,11, 4, 2, 1,12, 7, 4,10, 7,11,13, 6, 1,
                     8, 5, 5, 0, 3,15,15,10,13, 3, 0, 9,14, 8, 9, 6,
                     4,11, 2, 8, 1,12,11, 7,10, 1,13,14, 7, 2, 8,13,
                    15, 6, 9,15,12, 0, 5, 9, 6,10, 3, 4, 0, 5,14, 3
AND s6(b) = b!TABLE 12,10, 1,15,10, 4,15, 2, 9, 7, 2,12, 6, 9, 8, 5,
                     0, 6,13, 1, 3,13, 4,14,14, 0, 7,11, 5, 3,11, 8,
                     9, 4,14, 3,15, 2, 5,12, 2, 9, 8, 5,12,15, 3,10,
                     7,11, 0,14, 4, 1,10, 7, 1, 6,13, 0,11, 8, 6,13
AND s7(b) = b!TABLE  4,13,11, 0, 2,11,14, 7,15, 4, 0, 9, 8, 1,13,10,
                     3,14,12, 3, 9, 5, 7,12, 5, 2,10,15, 6, 8, 1, 6,
                     1, 6, 4,11,11,13,13, 8,12, 1, 3, 4, 7,10,14, 7,
                    10, 9,15, 5, 6, 0, 8,15, 0,14, 5, 2, 9, 3, 2,12
AND s8(b) = b!TABLE 13, 1, 2,15, 8,13, 4, 8, 6,10,15, 3,11, 7, 1, 4,
                    10,12, 9, 5, 3, 6,14,11, 5, 0, 0,14,12, 9, 7, 2,
                     7, 2,11, 1, 4,14, 1, 7, 9, 4,12,10,14, 8, 2,13,
                     0,15, 6,12,10, 9,13, 0,15, 3, 3, 5, 5, 6, 8,11

AND getbits(n, w, tab) = VALOF
{ LET res = 0
  FOR i = 0 TO n-1 DO res := res<<1 | (w>>(32-tab!i))&1
  RESULTIS res
}

AND getbits2(n, w0, w1, tab) = VALOF
{ LET res = 0
  FOR i = 0 TO n-1 DO 
  { LET sh = tab!i
    LET bit = sh<=32 -> w0>>32-sh & 1,
                        w1>>64-sh & 1
    res := res<<1 | bit
  }
  RESULTIS res
}

/* This program outputs the following:

Key:  00010011001101000101011101111001 10011011101111001101111111110001

K 1: 000110110000001011101111 111111000111000001110010
K 2: 011110011010111011011001 110110111100100111100101
K 3: 010101011111110010001010 010000101100111110011001
K 4: 011100101010110111010110 110110110011010100011101
K 5: 011111001110110000000111 111010110101001110101000
K 6: 011000111010010100111110 010100000111101100101111
K 7: 111011001000010010110111 111101100001100010111100
K 8: 111101111000101000111010 110000010011101111111011
K 9: 111000001101101111101011 111011011110011110000001
K10: 101100011111001101000111 101110100100011001001111
K11: 001000010101111111010011 110111101101001110000110
K12: 011101010111000111110101 100101000110011111101001
K13: 100101111100010111010001 111110101011101001000001
K14: 010111110100001110110111 111100101110011100111010
K15: 101111111001000110001101 001111010011111100001010
K16: 110010110011110110001011 000011100001011111110101

Encryption computation

M:    00000001001000110100010101100111 10001001101010111100110111101111
C:    10000101111010000001001101010100 00001111000010101011010000000101
encode: 0123456789ABCDEF with key: 133457799BBCDFF1 => 85E813540F0AB405

Decryption computation

C:    10000101111010000001001101010100 00001111000010101011010000000101
M:    00000001001000110100010101100111 10001001101010111100110111101111
decode: 85E813540F0AB405 with key: 133457799BBCDFF1 => 0123456789ABCDEF

*/
