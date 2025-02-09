SECTION "cobounce"

GET "libhdr"

GLOBAL {
  bounce_co : ug
  sender_co
}

LET start() BE 
{ LET argv = VEC 10
  LET count = 10_000_000

  UNLESS rdargs("COUNT", argv, 10) DO
  { writef("Bad arguments for SEND*n")
    stop(20)
  }

  IF argv!0 & string_to_number(argv!0) DO count := result2

  bounce_co := createco(bouncefn, 300)
  sender_co := createco(senderfn, 300)

  callco(sender_co, count)

  deleteco(sender_co)
  deleteco(bounce_co)
}

AND bouncefn(val) BE val := cowait(val) REPEAT

AND senderfn(count) BE
{ LET t = sys(Sys_cputime)
  writef("Calling the bounce coroutine %n times*n", count)
//abort(1000)
  FOR i = 1 TO count DO callco(bounce_co, i)
  t := sys(Sys_cputime) - t // Run time is msecs
  writef("About %n coroutine changes per second*n",
         muldiv(2*count, 1_000, t))
  writes("done*n")
}
