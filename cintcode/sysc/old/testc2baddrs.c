/*
This program tests various way of copying a C pointer of type
void * into one or two variables of type int or long int and then
recovering the original C pointer from the contents of the one
or 

#include <stdio.h>
#include "INT.h"
#include "cintsys.h"

int main() {
}

void copyaddrC2B(void*from, void*to) {
  // This function copies a machine address from a C program
  // to one or two BCPL words in a BCPL programs.  
#ifdef CURRENT64
#ifdef TARGET64
  // 64 bit machine address
  // 64 bit BCPL word
  // so copy one 64 bit value
  ((BCPLINT64*)to)[0] = ((BCPLINT64*)from)[0];
#else
  // 64 bit machine address
  // 32 bit BCPL word
  // so copy two 32 bit values
  ((BCPLINT32*)to)[0] = ((BCPLINT32*)from)[0];
  ((BCPLINT32*)to)[1] = ((BCPLINT32*)from)[1];
#endif
#else
#ifdef TARGET64
  // 32 bit machine address
  // 64 bit BCPL word
  // so copy the 32 bit machine address extended to 64 bits
  ((BCPLINT64*)to)[0] = (BCPLINT64)(((BCPLINT32*)from)[0]);
#else
  // 32 bit machine address
  // 32 bit BCPL word
  // so copy one 32 bit value
  ((BCPLINT32*)to)[0] = ((BCPLINT32*)from)[0];
#endif
#endif
}

void copyaddrB2C(void*from, void*to) {
  // This function copies data from one or two BCPL to
  // a C variable holding a machine address pointed to
  // by the second argument.
#ifdef CURRENT64
#ifdef TARGET64
  // 64 bit machine address
  // 64 bit BCPL word
  // so copy one 64 bit value
  ((BCPLINT64*)to)[0] = ((BCPLINT64*)from)[0];
#else
  // 64 bit machine address
  // 32 bit BCPL word
  // so copy two 32 bit values
  ((BCPLINT32*)to)[0] = ((BCPLINT32*)from)[0];
  ((BCPLINT32*)to)[1] = ((BCPLINT32*)from)[1];
#endif
#else
  // 32 bit machine address
  // 32 or 64 bit BCPL word
  // so copy a 32 bit value to the C variable
  ((BCPLINT32*)to)[0] = ((BCPLINT32*)from)[0];
#endif
}

