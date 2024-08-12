
//GET "libhdr.h"

LET start() = VALOF
  { LET x = 0
    FOR i = 1 BY -3 DO x := x+i
    RESULTIS x
  }

