GET "libhdr"

// A simple one line comment

LET start() = VALOF
{ // Another simple comment
  writef("Hello world*n")
  RESULTIS 0
}

/*
Here is a multi line comment
containing nested comments
// A nested comment
/* Another one */
last line of nested comment
*/

AND f(x) = x+1
