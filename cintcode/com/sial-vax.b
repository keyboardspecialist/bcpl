/*
This is an sial to vax code generator
implemented by Martin Richards (c) 7 August 2009

Vax register usage

R11   G, m/c address of global 0
R10   P, m/c address of the current function stack frame
R9    =0 (to simplify indirection and logical right shift)
R8    Sial A register
R7    Sial B register
R6    Sial C register
R2    Function entry point at time of call
R1    P pointer increment


P stack frame

...   <old P> <return address> <entry address> <arg 1> ...
        ^
        |
        P (=R10)

SP stack frame on entry to a BCPL function

      <return address>  ...
       ^
       |
       SP

BCPL Calling sequence

    KPG k n    =>      First argument, if any, in A (R8)
                       MOVL  4*n(R11),R2      ; R2 := G!n
                       MOVAL 4*k(R10),R1      ; R1 := <new P>
                       JSB   (R2)             ; J to function


    ENTRY n Lm C1 ... Cn =>
                                              ; first arg, if any, in A (=R8)
                                              ; sp!0 = <return address>
                       MOVL R10,(R1)          ; push <old P>
                       MOVL R1,R10            ; P := <new P>
                       MOVL (SP)+,4*1(R10)    ; save <return address>
                       MOVL R2,4*2(R10)       ; save <entry address>
                       MOVL R8,4*3(R10)       ; Store first arg, if any
                                              ; Other args in P!1, P!2, ...

    RTN     =>         MOVL 4(R10),R2         ; R2 = <return address>
                       MOVL (R10),R10         ; P := <old P>
                       JMP (R2)               ; Jump to <return address>


*/

SECTION "sial-vax"

GET "libhdr"
GET "sial.h"

GLOBAL {
sialin:ug
asmout
stdin
stdout
modstarted

rdf; rdp; rdg; rdk; rdw; rdl; rdc
rdcode

pval; gval; kval; wval; lval; mval

scan
cvf; cvfp; cvfg; cvfk; cvfw; cvfl

sectname; modletter; charv; labnumber
}

LET start() = VALOF
{ LET argv = VEC 20
  LET v    = VEC 20
  LET cv   = VEC 256/bytesperword

  sectname := v
  sectname%0 := 0
  modstarted := FALSE

  modletter := 'A'
  charv := cv
  labnumber := 0

  asmout := 0
  stdout := output()
  IF rdargs("FROM,TO/K", argv, 20)=0 DO
  { writes("Bad args for sial-vax*n")
    RESULTIS 20
  }
  IF argv!0=0 DO argv!0 := "prog.sial"
  IF argv!1=0 DO argv!1 := "prog.mar"
  sialin := findinput(argv!0)
  IF sialin=0 DO
  { writef("Trouble with file %s*n", argv!0)
    RESULTIS 20
  }
  asmout := findoutput(argv!1)
   
  UNLESS asmout DO
  { writef("Trouble with file %s*n", argv!1)
     RESULTIS 20
  }
   
  writef("Converting %s to %s*n", argv!0, argv!1)
  selectinput(sialin)
  selectoutput(asmout)

  writef("; Code generated by sial-vax*n*n")

  scan()
  endread()
  UNLESS asmout=stdout DO endwrite()
  selectoutput(stdout)
  writef("Conversion complete*n")
  RESULTIS 0
}

AND nextlab() = VALOF
{ labnumber := labnumber+1
  RESULTIS labnumber
}

// argument may be of form Ln
AND rdcode(let) = VALOF
{ LET a, ch, neg = 0, ?, FALSE

  ch := rdch() REPEATWHILE ch='*s' | ch='*n'

  IF ch=endstreamch RESULTIS -1

  UNLESS ch=let DO error("Bad item, looking for %c found %c*n", let, ch)

  ch := rdch()

  IF ch='-' DO { neg := TRUE; ch := rdch() }

  WHILE '0'<=ch<='9' DO { a := 10*a + ch - '0'; ch := rdch()  }

  RESULTIS neg -> -a, a
}

AND rdf() = rdcode('F')
AND rdp() = VALOF { pval := rdcode('P'); RESULTIS pval }
AND rdg() = VALOF { gval := rdcode('G'); RESULTIS gval }
AND rdk() = VALOF { kval := rdcode('K'); RESULTIS kval }
AND rdw() = VALOF { wval := rdcode('W'); RESULTIS wval }
AND rdl() = VALOF { lval := rdcode('L'); RESULTIS lval }
AND rdm() = VALOF { mval := rdcode('M'); RESULTIS mval }
AND rdc() = rdcode('C')

AND error(mess, a, b, c) BE
{ LET out = output()
  UNLESS out=stdout DO
  { selectoutput(stdout)
    writef(mess, a, b, c)
    selectoutput(out)
  }
  writef(mess, a, b, c)
}

AND scan() BE
{ LET op = rdf()

  IF op=-1 RETURN // EOF reached

  UNLESS modstarted DO
  { WHILE op=f_section | op=f_modstart TEST op=f_section
    THEN { cvfs("SECTION") // Name of section
           FOR i = 0 TO charv%0 DO sectname%i := charv%i
           op := rdf()
         }
    ELSE { cvf("MODSTART") // Start of module
           op := rdf()
         }

    // If there is no section name this module is called MAIN
    IF sectname%0=0 DO
    { LET s = "MAIN"
      FOR i = 0 TO s%0 DO sectname%i := s%i
    }

    writef("*n .PSECT %s,LONG", sectname)
    writef("*n .TITLE %s generated by sial-vax", sectname)
    writef("*n;%s::", sectname)
    writef("*n .LONG L1000")
    modstarted := TRUE
  }

  SWITCHON op INTO

  { DEFAULT:       error("; Bad op %n*n", op); LOOP

    CASE -1:       RETURN
      
    CASE f_lp:     cvfp("LP") // a := P!n
                   writef("*n MOVL %n(R10),R8", 4*pval)
                   ENDCASE
    CASE f_lg:     cvfg("LG") // a := G!n
                   writef("*n MOVL %n(R11),R8", 4*gval)
                   ENDCASE
    CASE f_ll:     cvfl("LL") // a := !Ln
                   writef("*n MOVL L%n,R8", lval)
                   ENDCASE

    CASE f_llp:    cvfp("LLP") // a := @ P!n
                   writef("*n MOVAL %n(R10),R8", 4*pval)
                   writef("*n DIVL2 #4,R8")
                   ENDCASE
    CASE f_llg:    cvfg("LLG") // a := @ G!n
                   writef("*n MOVAL %n(R11),R8", 4*gval)
                   writef("*n DIVL2 #4,R8")
                   ENDCASE
    CASE f_lll:    cvfl("LLL") // a := @ !Ln
                   writef("*n MOVAL L%n,R8", lval)
                   writef("*n DIVL2 #4,R8")
                   ENDCASE
    CASE f_lf:     cvfl("LF") // a := byte address of Ln
                   writef("*n MOVAL L%n,R8", lval)
                   ENDCASE
    CASE f_lw:     cvfm("LW")
                   writef("*n MOVL L%n,R8", mval+10000)
                   ENDCASE

    CASE f_l:      cvfk("L") // a := n
                   IF kval=0 DO { writef("*n CLRL R8"); ENDCASE }
                   writef("*n MOVL #%n,R8", kval)
                   ENDCASE
    CASE f_lm:     cvfk("LM") // a := -n
                   writef("*n MOVL #-%n,R8", kval)
                   ENDCASE

    CASE f_sp:     cvfp("SP") // P!n := a
                   writef("*n MOVL R8,%n(R10)", 4*pval)
                   ENDCASE
    CASE f_sg:     cvfg("SG") // G!n := a
                   writef("*n MOVL R8,%n(R11)", 4*gval)
                   ENDCASE
    CASE f_sl:     cvfl("SL") // !Ln := a
                   writef("*n MOVL R8,L%n", lval)
                   ENDCASE

    CASE f_ap:     cvfp("AP") // a := a + P!n
                   writef("*n ADDL2 %n(R10),R8", 4*pval)
                   ENDCASE
    CASE f_ag:     cvfg("AG") // a := a + G!n
                   writef("*n ADDL2 %n(R11),R8", 4*gval)
                   ENDCASE
    CASE f_a:      cvfk("A") // a := a + n
                   IF kval=0 ENDCASE
                   IF kval=1  DO { writef("*n INCL R8"); ENDCASE }
                   IF kval=-1 DO { writef("*n DECL R8"); ENDCASE }
                   writef("*n ADDL2 #%n,R8", kval)
                   ENDCASE
    CASE f_s:      cvfk("S")  // a := a - k
                   IF kval=0 ENDCASE
                   IF kval=1  DO { writef("*n DECL R8"); ENDCASE }
                   IF kval=-1 DO { writef("*n INCL R8"); ENDCASE }
                   writef("*n SUBL2 #%n,R8", kval)
                   ENDCASE

    CASE f_lkp:    cvfkp("LKP") // a := P!n!k
                   writef("*n MOVL %n(R10),R1", 4*pval)
                   writef("*n MOVL %n(R9)[R1],R8", 4*kval)
                   ENDCASE
    CASE f_lkg:    cvfkg("LKG") // a := G!n!k
                   writef("*n MOVL %n(R11),R1", 4*gval)
                   writef("*n MOVL %n(R9)[R1],R8", 4*kval)
                   ENDCASE
    CASE f_rv:     cvf("RV")  // a := ! a
                   writef("*n MOVL (R9)[R8],R8")
                   ENDCASE
    CASE f_rvp:    cvfp("RVP") // a := P!n!a
                   writef("*n ADDL2 %n(R10),R8", 4*pval)
                   writef("*n MOVL (R9)[R8],R8")
                   ENDCASE
    CASE f_rvk:    cvfk("RVK") // a := a!k
                   writef("*n MOVL %n(R9)[R8],R8", 4*kval)
                   ENDCASE
    CASE f_st:     cvf("ST") // !a := b
                   writef("*n MOVL R7,(R9)[R8]")
                   ENDCASE
    CASE f_stp:    cvfp("STP") // P!n!a := b
                   writef("*n MOVL %n(R10),R1", 4*pval)
                   writef("*n ADDL2 R8,R1")
                   writef("*n MOVL R7,(R9)[R1]")
                   ENDCASE
    CASE f_stk:    cvfk("STK") // a!k := b
                   writef("*n MOVL R7,%n(R9)[R8]", 4*kval)
                   ENDCASE
    CASE f_stkp:   cvfkp("STKP")  // P!n!k := a
                   writef("*n MOVL %n(R10),R1", 4*pval)
                   writef("*n MOVL R8,%n(R9)[R1]", 4*kval)
                   ENDCASE
    CASE f_skg:    cvfkg("SKG") // G!n!k := a
                   writef("*n MOVL %n(R11),R1", 4*gval)
                   writef("*n MOVL R8,%n(R9)[R1]", 4*kval)
                   ENDCASE
    CASE f_xst:    cvf("XST") // !b := a
                   writef("*n MOVL R8,(R9)[R7]")
                   ENDCASE

    CASE f_k:      cvfp("K") // Call  a(b,...) incrementing P by n
                   writef("*n MOVL R8,R2")   // R2 = <entry point>
                   writef("*n MOVL R7,R8")   // R8 = <first arg>, if any
                   writef("*n MOVAL %n(R10),R1", 4*pval) // R1 = <new P>
                   writef("*n JSB (R2)")     // Subroutine jump
                   ENDCASE

    CASE f_kpg:    cvfpg("KPG") // Call Gg(a,...) incrementing P by k
                   writef("*n MOVL %n(R11),R2", 4*gval)
                   writef("*n MOVAL %n(R10),R1", 4*pval)
                   writef("*n JSB (R2)")
                   ENDCASE

    CASE f_neg:    cvf("NEG") // a := - a
                   writef("*n MNEGL R8,R8") 
                   ENDCASE
    CASE f_not:    cvf("NOT") // a := ~ a
                   writef("*n MCOML R8,R8") 
                   ENDCASE
    CASE f_abs:    cvf("ABS") // a := ABS a
                 { LET lab = nextlab()
                   writef("*n TSTL  R8")
                   writef("*n BGEQ  X%n", lab)
                   writef("*n MNEGL R8,R8")
                   writef("*nX%n:", lab)
                   ENDCASE
                 }

    CASE f_xdiv:   cvf("XDIV") // a := a / b
                   writef("*n DIVL2 R7,R8")
                   ENDCASE
    CASE f_xrem:   cvf("XREM") // a := a REM b
                   writef("*n MOVL  R8,R3")
                   writef("*n ASHQ  #-32,R2,R2")
                   writef("*n EDIV  R7,R2,R8,R4")
                   writef("*n MOVL  R4,R8")
                   ENDCASE
    CASE f_xsub:   cvf("XSUB") // a :=  a - b
                   writef("*n SUBL2 R7,R8")
                   ENDCASE

    CASE f_mul:    cvf("MUL") // a := b * a; c := ?
                   writef("*n MULL2 R7,R8")
                   ENDCASE
    CASE f_div:    cvf("DIV")  // a := b / a; c := ?
                   writef("*n DIVL3  R8,R7,R8")
                   ENDCASE
    CASE f_rem:    cvf("REM") // a := b REM a; c := ?
                   writef("*n MOVL  R7,R3")
                   writef("*n ASHQ  #-32,R2,R2")
                   writef("*n EDIV  R8,R2,R7,R4")
                   writef("*n MOVL  R4,R8")
                   ENDCASE
    CASE f_add:    cvf("ADD") // a := b + a
                   writef("*n ADDL2 R7,R8")
                   ENDCASE
    CASE f_sub:    cvf("SUB") // a := b - a
                   writef("*n SUBL3 R8,R7,R8")
                   ENDCASE

    CASE f_eq:     cvf("EQ") // a := b = a
                 { LET lab = nextlab()
                   writef("*n CLRL R4")
                   writef("*n CMPL R7,R8")
                    writef("*n BNEQ X%n", lab)
                    writef("*n DECL R4")
                    writef("*nX%n:", lab)
                   writef("*n MOVL R4,R8")
                   ENDCASE
                 }
    CASE f_ne:     cvf("NE") // a := b ~= a
                 { LET lab = nextlab()
                   writef("*n CLRL R4")
                   writef("*n CMPL R7,R8")
                   writef("*n BEQL X%n", lab)
                   writef("*n DECL R4")
                   writef("*nX%n:", lab)
                   writef("*n MOVL R4,R8")
                   ENDCASE
                 }
    CASE f_ls:     cvf("LS") // a := b < a
                 { LET lab = nextlab()
                   writef("*n CLRL R4")
                   writef("*n CMPL R7,R8")
                   writef("*n BGEQ X%n", lab)
                   writef("*n DECL R4")
                   writef("*nX%n:", lab)
                   writef("*n MOVL R4,R8")
                   ENDCASE
                 }
    CASE f_gr:     cvf("GR") // a := b > a
                 { LET lab = nextlab()
                   writef("*n CLRL R4")
                   writef("*n CMPL R7,R8")
                   writef("*n BLEQ X%n", lab)
                   writef("*n DECL R4")
                   writef("*nX%n:", lab)
                   writef("*n MOVL R4,R8")
                   ENDCASE
                 }
    CASE f_le:     cvf("LE") // a := b <= a
                 { LET lab = nextlab()
                   writef("*n CLRL R4")
                   writef("*n CMPL R7,R8")
                   writef("*n BGTR X%n", lab)
                   writef("*n DECL R4")
                   writef("*nX%n:", lab)
                   writef("*n MOVL R4,R8")
                   ENDCASE
                 }
    CASE f_ge:     cvf("GE") // a := b >= a
                 { LET lab = nextlab()
                   writef("*n CLRL R4")
                   writef("*n CMPL R7,R8")
                   writef("*n BLSS X%n", lab)
                   writef("*n DECL R4")
                   writef("*nX%n:", lab)
                   writef("*n MOVL R4,R8")
                   ENDCASE
                 }
    CASE f_eq0:    cvf("EQ0") // a := a = 0
                 { LET lab = nextlab()
                   writef("*n CLRL R4")
                   writef("*n TSTL R8")
                   writef("*n BNEQ X%n", lab)
                   writef("*n DECL R4")
                   writef("*nX%n:", lab)
                   writef("*n MOVL R4,R8")
                   ENDCASE
                 }
    CASE f_ne0:    cvf("NE0") // a := a ~= 0
                 { LET lab = nextlab()
                   writef("*n CLRL R4")
                   writef("*n TSTL R8")
                   writef("*n BEQL X%n", lab)
                   writef("*n DECL R4")
                   writef("*nX%n:", lab)
                   writef("*n MOVL R4,R8")
                   ENDCASE
                 }
    CASE f_ls0:    cvf("LS0") // a := a < 0
                   writef("*n ASHL #-32,R8,R8")
                   ENDCASE
    CASE f_gr0:    cvf("GR0") // a := a > 0
                   writef("*n SUBL3 #1,R8,R4")
                   writef("*n BISL2 R8,R4")
                   writef("*n ASHL #-32,R4,R8")
                   writef("*n MCOML R8,R8")
                   ENDCASE
    CASE f_le0:    cvf("LE0") // a := a <= 0
                   writef("*n SUBL3 #1,R8,R4")
                   writef("*n BISL2 R8,R4")
                   writef("*n ASHL #-32,R4,R8")
                   ENDCASE
    CASE f_ge0:    cvf("GE0") // a := a >= 0
                   writef("*n ASHL #-32,R8,R8")
                   writef("*n MCOML R8,R8")
                   ENDCASE

    CASE f_lsh:    cvf("LSH") // a := b << a; b := ?
                   writef("*n ASHL R8,R7,R8")
                   ENDCASE
    CASE f_rsh:    cvf("RSH") // a := b >> a; b := ?
                   writef("*n MNEGL R8,R4")
                   writef("*n MOVL R7,R8")
                   writef("*n ASHQ R4,R8,R8")
                   ENDCASE
    CASE f_and:    cvf("AND") // a := b & a
                   writef("*n MCOML R7,R1") 
                   writef("*n BICL2 R1,R8") 
                   ENDCASE
    CASE f_or:     cvf("OR") // a := b | a 
                   writef("*n BISL2 R7,R8") 
                   ENDCASE
    CASE f_xor:    cvf("XOR") // a := b NEQV a
                   writef("*n XORL2 R7,R8") 
                   ENDCASE
    CASE f_eqv:    cvf("EQV") // a := b EQV a 
                   writef("*n XORL2 R7,R8") 
                   writef("*n MCOML R8,R8") 
                   ENDCASE

    CASE f_gbyt:   cvf("GBYT") // a := b % a
                   writef("*n MULL3 #4,R7,R4") 
                   writef("*n MOVZBL (R4)[R8],R8") 
                   ENDCASE
    CASE f_xgbyt:  cvf("XGBYT") // a := a % b 
                   writef("*n MULL3 #4,R8,R4") 
                   writef("*n MOVZBL (R4)[R7],R8") 
                   ENDCASE
    CASE f_pbyt:   cvf("PBYT") // b % a := c
                   writef("*n MULL3 #4,R7,R4") 
                   writef("*n MOVB R6,(R4)[R8]") 
                   ENDCASE
    CASE f_xpbyt:  cvf("XPBYT") // a % b := c 
                   writef("*n MULL3 #4,R8,R4") 
                   writef("*n MOVB R6,(R4)[R7]") 
                   ENDCASE

// swb       Kn Ld K1 L1 ... Kn Ln   Binary chop switch, Ld default
    CASE f_swb:    cvswb()
                   ENDCASE

// swl       Kn Ld L1 ... Ln         Label vector switch, Ld default
    CASE f_swl:    cvswl()
                   ENDCASE

    CASE f_xch:    cvf("XCH") // swap a and b
                   writef("*n MOVL R8,R4")
                   writef("*n MOVL R7,R8")
                   writef("*n MOVL R4,R7")
                   ENDCASE
    CASE f_atb:    cvf("ATB") // b := a
                   writef("*n MOVL R8,R7")
                   ENDCASE
    CASE f_atc:    cvf("ATC") // c := a
                   writef("*n MOVL R8,R6")
                   ENDCASE
    CASE f_bta:    cvf("BTA") // a := b
                   writef("*n MOVL R7,R8")
                   ENDCASE
    CASE f_btc:    cvf("BTC") // c := b
                   writef("*n MOVL R7,R6")
                   ENDCASE
    CASE f_atblp:  cvfp("ATBLP") // b := a; a := P!n
                   writef("*n MOVL R8,R7")
                   writef("*n MOVL %n(R10),R8", 4*pval)
                   ENDCASE
    CASE f_atblg:  cvfg("ATBLG") // b := a; a := G!n
                   writef("*n MOVL R8,R7")
                   writef("*n MOVL %n(R11),R8", 4*gval)
                   ENDCASE
    CASE f_atbl:   cvfk("ATBL") // b := a; a := k
                   writef("*n MOVL R8,R7")
                   writef("*n MOVL #%n,R8", kval)
                   ENDCASE

    CASE f_j:      cvfl("J") // jump to Ln
                   writef("*n JMP L%n", lval)
                   ENDCASE
    CASE f_rtn:    cvf("RTN") // procedure return
                   writef("*n MOVL 4(R10),R2")  // R2 = <return address>
                   writef("*n MOVL (R10),R10")  // P := old P
                   writef("*n JMP (R2)")        // Jump to <return address>
                   ENDCASE

    CASE f_goto:   cvf("GOTO") // jump to a
                   writef("*n JMP (R8)")
                   ENDCASE

    CASE f_res:    cvf("RES")   // <res> := A
                                // <res> is already in A
                                // nothing to do
                   ENDCASE

    CASE f_ldres:  cvf("LSRES") // A := <res>
                                // A and B already set properly
                   ENDCASE

    CASE f_ikp:    cvfkp("IKP") // a := P!n + k; P!n := a
                   writef("*n MOVL %n(R10),R8", 4*pval)
                   TEST kval=1
                   THEN writef("*n INCL R8")
                   ELSE TEST kval=-1
                        THEN writef("*n DECL R8")
                        ELSE writef("*n ADDL2 #%n,R8", kval)
                   writef("*n MOVL R8,%n(R10)", 4*pval)
                   ENDCASE
    CASE f_ikg:    cvfkg("IKG") // a := G!n + k; G!n := a
                   writef("*n MOVL %n(R11),R8", 4*gval)
                   TEST kval=1
                   THEN writef("*n INCL R8")
                   ELSE TEST kval=-1
                        THEN writef("*n DECL R8")
                        ELSE writef("*n ADDL2 #%n,R8", kval)
                   writef("*n MOVL R8,%n(R11)", 4*gval)
                   ENDCASE
    CASE f_ikl:    cvfkl("IKL") // a := !Ln + k; !Ln := a
                   writef("*n MOVL L%n,R8", lval)
                   TEST kval=1
                   THEN writef("*n INCL R8")
                   ELSE TEST kval=-1
                        THEN writef("*n DECL R8")
                        ELSE writef("*n ADDL2 #%n,R8", kval)
                   writef("*n MOVL R8,L%n", lval)
                   ENDCASE
    CASE f_ip:     cvfp("IP") // a := P!n + a; P!n := a
                   writef("*n ADDL2 %n(R10),R8", 4*pval)
                   writef("*n MOVL R8,%n(R10)", 4*pval)
                   ENDCASE
    CASE f_ig:     cvfg("IG") // a := G!n + a; G!n := a
                   writef("*n ADDL2 %n(R11),R8", 4*gval)
                   writef("*n MOVL R8,%n(R11)", 4*gval)
                   ENDCASE
    CASE f_il:     cvfl("IL") // a := !Ln + a; !Ln := a
                   writef("*n ADDL2 L%n,R8", lval)
                   writef("*n MOVL R8,L%n", lval)
                   ENDCASE

    CASE f_jeq:    cvfl("JEQ") // Jump to Ln if b = a
                   writef("*n CMPL R7,R8")
                   genBEQL(lval)
                   ENDCASE
    CASE f_jne:    cvfl("JNE") // Jump to Ln if b ~= a
                   writef("*n CMPL R7,R8")
                   genBNEQ(lval)
                   ENDCASE
    CASE f_jls:    cvfl("JLS") // Jump to Ln if b < a
                   writef("*n CMPL R7,R8")
                   genBLSS(lval)
                   ENDCASE
    CASE f_jgr:    cvfl("JGR") // Jump to Ln if b > a
                   writef("*n CMPL R7,R8")
                   genBGTR(lval)
                   ENDCASE
    CASE f_jle:    cvfl("JLE") // Jump to Ln if b <= a
                   writef("*n CMPL R7,R8")
                   genBLEQ(lval)
                   ENDCASE
    CASE f_jge:    cvfl("JGE") // Jump to Ln if b >= a
                   writef("*n CMPL R7,R8")
                   genBGEQ(lval)
                   ENDCASE

    CASE f_jeq0:   cvfl("JEQ0") // Jump to Ln if a = 0
                   writef("*n TSTL R8")
                   genBEQL(lval)
                   ENDCASE
    CASE f_jne0:   cvfl("JNE0") // Jump to Ln if a ~= 0
                   writef("*n TSTL R8")
                   genBNEQ(lval)
                   ENDCASE
    CASE f_jls0:   cvfl("JLS0") // Jump to Ln if a < 0
                   writef("*n TSTL R8")
                   genBLSS(lval)
                   ENDCASE
    CASE f_jgr0:   cvfl("JGR0") // Jump to Ln if a > 0
                   writef("*n TSTL R8")
                   genBGTR(lval)
                   ENDCASE
    CASE f_jle0:   cvfl("JLE0") // Jump to Ln if a <= 0
                   writef("*n TSTL R8")
                   genBLEQ(lval)
                   ENDCASE
    CASE f_jge0:   cvfl("JGE0") // Jump to Ln if a >= 0
                   writef("*n TSTL R8")
                   genBGEQ(lval)
                   ENDCASE
    CASE f_jge0m:  cvfm("JGE0M") // Jump to Mn if a >= 0
                   writef("*n TSTL R8")
                   genBGEQ(mval+10000)
                   ENDCASE

    // The following five opcodes are never generated by
    // the BCPL compiler
    CASE f_brk:    cvf("BRK") // Breakpoint instruction
                   writef("*n unimplemented")
                   ENDCASE
    CASE f_nop:    cvf("NOP") // No operation
                   ENDCASE
    CASE f_chgco:  cvf("CHGCO") // Change coroutine
                   writef("*n unimplemented")
                   ENDCASE
    CASE f_mdiv:   cvf("MDIV") // a := Muldiv(P!3, P!4, P!5) 
                   writef("*n unimplemented")
                   ENDCASE
    CASE f_sys:    cvf("SYS") // System function
                   writef("*n unimplemented")
                   ENDCASE

    CASE f_modend:   cvf("MODEND") // End of module
                     writef("*nL1000:") 
                     writef("*n .END*n") 
                     modletter := modletter+1
                     modstarted := FALSE
                     ENDCASE
    CASE f_global:   cvglobal() // Global initialisation data
                     ENDCASE
    CASE f_string:   cvstring() // String constant
                     ENDCASE
    CASE f_const:    cvconst() // Large integer constant
                     ENDCASE

    CASE f_static:   cvstatic() // Static variable or table
                     ENDCASE
    CASE f_mlab:     cvfm("MLAB") // Destination of jge0m
                     writef("*nL%n:", mval+10000)
                     ENDCASE
    CASE f_lab:      cvfl("LAB") // Program label
                     writef("*nL%n:", lval)
                     ENDCASE
    CASE f_lstr:     cvfm("LSTR") // a := Mn   (pointer to string)
                     writef("*n MOVAL W^L%n,R8", mval+10000)
                     writef("*n DIVL2 #4,R8")
                     ENDCASE
    CASE f_entry:    cventry() // Start of a function
                     ENDCASE
  }

  newline()
} REPEAT

AND cvf(s)   BE writef("; %s", s)
AND cvfp(s)  BE writef("; %t7 P%n", s, rdp())
AND cvfkp(s) BE writef("; %t7 K%n P%n", s, rdk(), rdp())
AND cvfg(s)  BE writef("; %t7 G%n", s, rdg())
AND cvfkg(s) BE writef("; %t7 K%n G%n", s, rdk(), rdg())
AND cvfkl(s) BE writef("; %t7 K%n L%n", s, rdk(), rdl())
AND cvfpg(s) BE writef("; %t7 P%n G%n", s, rdp(), rdg())
AND cvfk(s)  BE writef("; %t7 K%n", s, rdk())
AND cvfw(s)  BE writef("; %t7 W%n", s, rdw())
AND cvfl(s)  BE writef("; %t7 L%n", s, rdl())
AND cvfm(s)  BE writef("; %t7 M%n", s, rdm())

AND cvswl() BE
{ LET n = rdk()
  LET dlab = rdl()
  LET lab = nextlab()
  writef("; SWL K%n L%n", n, dlab)
  writef("*n TSTL R8")
  genBLSS(dlab)
  writef("*n CMPL #%n,R8", n)
  genBLEQ(dlab)
writef("*n MOVL L^X%n(R9)[R8],R1", lab)
writef("*n JMP (R1)")
//  writef("*n JMP L^X%n(R9)[R8]", lab)
  writef("*n .ALIGN LONG")
  writef("*nX%n:", lab)
  FOR i = 1 TO n DO
  { writef("*n; L%n", rdl())
    writef("*n .LONG L%n", lval)
  }
}

AND cvswb() BE
{ LET n = rdk()
  LET dlab = rdl()
  writef("; SWB K%n L%n", n, dlab)  // A naive implementation of swb
  FOR i = 1 TO n DO 
  { LET k = rdk()
    LET lab = rdl()
    writef("*n; K%n L%n", k, lab)
    writef("*n CMPL #%n,R8", k)
    genBEQL(lab)
  }
  writef("*n JMP L%n", dlab)
}

AND cvglobal() BE
{ LET n = rdk()
  writef("; GLOBAL K%n*n", n)
  IF sectname%0=0 FOR i = 0 TO 4 DO sectname%i := "prog"%i
  writef("*n;.globl %s*n", sectname)
  writef(";%s::*n", sectname)
  writef(" .entry %s,0*n", sectname)
  writef(" movl 4(ap),r1*n")
  FOR i = 1 TO n DO
  { LET g = rdg()
    LET n = rdl()
    writef("; G%n L%n*n", g, n)
    writef(" MOVAL L%n,%n(r1)*n", n, 4*g)
  }
  writef("; G%n", rdg())
  writef("*n RET*n")
}

AND cvglobal1() BE
{ LET n = rdk()
  writef("*n; GLOBAL K%n", n)
  IF sectname%0=0 FOR i = 0 TO 4 DO sectname%i := "prog"%i
  writef("*n.ALIGN LONG")
  writef("*n.LONG -1")
  FOR i = 1 TO n DO
  { LET g = rdg()
    LET n = rdl()
    writef("*n; G%n L%n*n", g, n)
    writef("*n .LONG %i4, L%n", g, n)
  }
  writef("*n; G%n", rdg())
  writef("*n .LONG %i4", gval)
}

AND rdchars() = VALOF
{ LET n = rdk()
  charv%0 := n
  FOR i = 1 TO n DO charv%i := rdc()
  RESULTIS n
}

AND cvstring() BE
{ LET lab = rdm()
  LET n = rdchars()
  writef("; STRING  M%n K%n", lab, n)
  FOR i = 1 TO n DO writef(" C%n", charv%i)
  writef("*n .ALIGN LONG")
  writef("*nL%n:", lab+10000)
  FOR i = 0 TO n DO writef("*n .BYTE %n", charv%i)
}

AND cvconst() BE
{ LET lab = rdm()
  LET w = rdw()
  writef("; CONST   M%n W%n", lab, w)
  writef("*n .ALIGN LONG")
  writef("*nL%n:", lab+10000)
  writef("*n .LONG %n", w)
}

AND cvstatic() BE
{ LET lab = rdl()
  LET n = rdk()
  writef("; STATIC  L%n K%n", lab, n)
  writef("*n .ALIGN LONG")
  writef("*nL%n:", lab)
  FOR i = 1 TO n DO { writef("*n; W%n", rdw())
                      writef("*n .LONG %n", wval)
                    }
}

AND cvfs(s) BE
{ LET n = rdchars()
  writef("; %t7 K%n", s, n)
  FOR i = 1 TO n DO writef(" C%n", charv%i)
}

AND cventry() BE
{ LET n = rdchars()
  LET op = rdf()
  LET lab = rdl()
  writef("*n*n; Entry to: %s*n", charv)
  writef("; %t7 K%n", "ENTRY", n)
  FOR i = 1 TO n DO writef(" C%n", charv%i)
  newline()
  TEST op=f_lab THEN writef("; LAB     L%n*n", lab)
                ELSE writef("; Bad op F%n L%n*n", op, lab)
  FOR i = n+1 TO 11 DO charv%i := ' '
  IF n>11 DO charv!0 := 11
  writef("*n .LONG ^X%x8",entryword)
  writef("*n .LONG ^X%x8",charv!0)
  writef("*n .LONG ^X%x8",charv!1)
  writef("*n .LONG ^X%x8",charv!2)

  writef("*nL%n:", lab)
  writef("*n MOVL R10,(R1)")       // Save <old P>
  writef("*n MOVL R1,R10")         // P := <new P>
  writef("*n MOVL (SP)+,4(R10)") // Save <return address>
  writef("*n MOVL R2,8(R10)")    // Save <entry point>
  writef("*n MOVL R8,12(R10)")    // Save first argument
}

AND genBEQL(lab) BE
{ LET m = nextlab()
  writef("*n BNEQ X%n", m)
  writef("*n JMP L%n", lab)
  writef("*nX%n:", m)
}

AND genBNEQ(lab) BE
{ LET m = nextlab()
  writef("*n BEQL X%n", m)
  writef("*n JMP L%n", lab)
  writef("*nX%n:", m)
}

AND genBLSS(lab) BE
{ LET m = nextlab()
  writef("*n BGEQ X%n", m)
  writef("*n JMP L%n", lab)
  writef("*nX%n:", m)
}

AND genBGTR(lab) BE
{ LET m = nextlab()
  writef("*n BLEQ X%n", m)
  writef("*n JMP L%n", lab)
  writef("*nX%n:", m)
}

AND genBLEQ(lab) BE
{ LET m = nextlab()
  writef("*n BGTR X%n", m)
  writef("*n JMP L%n", lab)
  writef("*nX%n:", m)
}

AND genBGEQ(lab) BE
{ LET m = nextlab()
  writef("*n BLSS X%n", m)
  writef("*n JMP L%n", lab)
  writef("*nX%n:", m)
}


