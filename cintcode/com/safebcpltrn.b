// This is the translation phase of the BCPL compiler.

// Implemented by Martin Richards (c) 12 Feb 2022

/*

Change history

27/06/2022
Removed the global variables ret0lab and retlab, using code dependent
on proccontect instead.

08/06/2022
Changed the meaning of nextlab used in the compilation of NEXT.

nextlab = -2    NEXT is illegal
nextlab >  0    NEXT is campiled as JUMP n
nextlab =  0    This happens when translating the last match item of
                a match list, and NEXT is compiled as if it were EXIT.

30/05/2022
Changed the meaning of parameter next in trans. If next=-1 compile
code to return from the current function or routine based on the
vales of retlab and ret0lab. Reimplemented the translation of
BREAK, LOOP, ENDCASE, NEXT, EXIT, DEFAULT, RESULTIS and RETURN.

22/10/2021
The old BCPL front end bcplfe.b has just be separated into
two separate sections bcplsyn.b and bcpltrn.b because the section
size was becoming to large for Cintcode.

*/

SECTION "TRN"

GET "libhdr"
GET "bcplfecg"

// Global declarations for TRN

GLOBAL  {
// Globals used by both bcplsyn.b and bcpltrn.b
sourcenamev:feg
sourcefileno; sourcenamevupb


// Globals only used by bcpltrn.b
trnext:trng
trans; declnames; decldyn
declstat; checkdistinct; addname; cellwithname
transdef; scanlabel
transmatchlist
nextmatchitem
transbreak; transloop; transendcase
transnext; transexit; transreturn
condbreak; condloop; condendcase
condnext; condexit; condreturn
decllabels; undeclare
jumpcond; transswitch; transfor
assop2op; op2sfop; cv2flt; rel2patrel; patrel2rel
assign; load; fnbody; loadlv; loadlist
isflt; isconst; iszero; evalconst; transname; xref
genlab; labnumber
newblk
dvec; dvece; dvecp; dvect

caselist      // Linked list of case constants of the current switch
casecount     // If =-1 CASE and DEFAULT labels are illegal,
              // but ENDCASE may be allowed, eg in a FOR loop
	      // within a SWITCHON command.

resultlab     // =-2 RESULTIS illegal
              // If =-1 RESULTIS compiles as
	      // load the result followed by FNRN
	      // This is used when a VALOF expression is the body
	      // of a function.
	      // If >=0 RESULTIS compiles as
	      // load the result followed by RES resultlab
	      // allocating resultlab if necessary.
	      // If the matchcontext is s_patfndef, match item
	      // expressions are treated as operands of RESULTIS.

defaultlab    // If defaultlab=-2 we are not in a SWITCHON command.
              // It is used to hold the label for DEFAULT,
	      // or the label to use if DEFAULT is not present.
	      // If =0 DEFAULT is legal but no lable allocated yet.
	      // If >0 the DEFAULT label has been encountered.
	      // If =-1 The was no DEFAULT label and next was -1
	      // when trans was called to compile the current
	      // SWITCHON command.

looplab       // =-2 LOOP is illegal, ie not in a repetitive command
              // > 0 LOOP compiles as a jump to this label
	      // looplab will never be 0 or -1.
              
breaklab      // =-2 BREAK is illegal, ie not in a repetitive command
              //     in the current procedure. 
              // >=0 BREAK compiles as a jump, allocating breaklab if necessary.
              // =-1 BREAK compiles code to return from the current procedure
	      //     The code will be RTRN or LN 0; FNRN.

endcaselab    // =-2 ENDCASE is illegal, ie not in a SWITCHON command.
              // >=0 ENDCASE compiles as a jump,
	      //     allocating endcaselab if necessary.
              // =-1 ENDCASE compiles code to return from
	      //     the current procedure, either RTRN or LN 0; FNRN.

exitlab       // =-2 EXIT is illegal, ie not in a match list.
              // >=0 EXIT compiles as a jump
	      //     allocating exitlab if necessary.
              // =-1 EXIT compiles code to return from
	      //     the current procedure. If proccontext is s_rtrn
	      //     the compiled code is RTRN.  If proccontext is s_fnrn
	      //     the code is LN 0 or LP patresultpos followed by FNRN.

nextlab       // =-2 NEXT is illegal, ie not in a match list.
              // > 0 NEXT compiles as a jump to nextlab.
              // = 0 This only occurs when NEXT is in the last match item
	      //     of a match list, and so is equivalent to EXIT.
	
proccontext   // =0  Not in a function or routine
              // otherwise =s_fnrn or =s_rtrn

choosereturnlab


context; comline; procname
matchcontext // Equals s_patfndef, s_patrtdef,
             //        s_matche,   s_matche,
	     //        s_everye,   s_everyc,
	     // otherwise zero when not compiling a match construct.

patresultpos // Position of the result of the current EVERY expression.
ssp          // The stack position just above the top item of the stack.
vecssp
gdeflist; gdefcount
outstring; out1; out2; out3; out4
outcomment
nocomments  // normally set to TRUE. Set in translate.

lasttrnglobal // Used to check for global overlap with cgg

}


LET genlab() = VALOF
{ labnumber := labnumber + 1
  RESULTIS labnumber
}
 
AND trnerr(mess, a, b, c) BE
{ LET fno = comline>>20
  LET lno = comline & #xFFFFF
  LET filename = sourcenamev!fno
  writes("Error ")
  UNLESS procname=0 DO writef("in %s %s ",
                               proccontext=s_rtrn -> "routine",
                               proccontext=s_fnrn -> "function", "",
			       @h3!procname)
  writef("near ")
  IF filename DO writef("%s", filename)
  writef("[%n]: ", lno)
  writef(mess, a, b, c)
  newline()
  IF hard DO abort(1000)
  errcount := errcount + 1
  IF errcount >= errmax DO { writes("*nCompilation aborted*n")
                             longjump(fin_p, fin_l)
                           }
}

AND newblk(x, y, z) = VALOF
{ // This is used for global and case lists.
  // It uses space at the end of the area allocated for
  // the environment vector.
  LET p = dvect - 3
  IF dvece>p DO { errmax := 0        // Make it fatal.
                  trnerr("More workspace needed")
                }
  p!0, p!1, p!2 := x, y, z
  dvect := p
  RESULTIS p
}

AND translate(x) = VALOF
{ // First check that the trn globals do not overlap with
  // the codegenerator globals.
  LET lasttrngn = @lasttrnglobal - @glob0

  nocomments := TRUE  // Normal setting
  //nocomments := FALSE // Setting when debugging
  
  IF debug>0 DO writef("lasttrngn=%i3   cgg=%i3*n", lasttrngn, cgg)

  IF lasttrngn>=cgg DO
  { writef("SYSTEM ERROR: lasttrngn=%i3   cgg=%i3*n", lasttrngn, cgg)
    abort(999)
    RESULTIS FALSE // Translation failure
  }
 
  dvec,  dvect := treevec, treep
  h1!dvec, h2!dvec, h3!dvec, h4!dvec := 0, 0, 0, 0
  dvece := dvec+4
  dvecp := dvece

  // Clear the h2 field of all names in the name table
  // This field will be used to point to the dvec cell for the current
  // declaration of the name.
  // Note that the size of dvec cells has been increased to 4 to allow
  // for path variables in the pattern matching extension.
  
  FOR i = 0 TO nametablesize-1 DO
  { LET name = nametable!i // The first name in hash list i
    UNTIL name=0 DO
    { LET next = h2!name
      h2!name := 0 // Mark undeclared
      name := next
    }
  }

  // We are in translate so initialise all the translation variables

  gdeflist, gdefcount := 0, 0
  caselist := 0
  casecount := -1 // Not in a SWITCHON command, so CASE and DEFAULT
                  // and illegal.
  
  defaultlab := -2
  resultlab, breaklab, looplab, endcaselab := -2, -2, -2, -2
  proccontext :=  0 // Not in a function or routine
  procname := 0 // This is zero when not compiling a function or routine.
  nextlab, exitlab := -2, -2 // Not in a pattern list
  matchcontext := 0  // Not in a match construct
  proccontext  := 0  // Not in a function or routine
  patresultpos := 0  // Only non zero when in an EVERY expression 

  context, comline := 0, 1
  labnumber := 9 // Make L10 the first usable label number
                 // smaller values may be used for  other purposes
  ssp, vecssp := savespacesize, savespacesize

  WHILE x & (h1!x=s_section | h1!x=s_needs) DO
  { LET op, a = h1!x, h2!x
    out1(op)
    outstring(@h2!a)
    x:=h3!x
  }

  trans(x, 0) // Since procname=0 this call translates the outer level
              // declarations and there is no need to compile jumps
	      // around the function or routine bodies.
	      // Such jumps are only needed for functions and routines
	      // defined inside other functions or routines.

  // Output the global initialisation code
  out2(s_global, gdefcount)
  UNTIL gdeflist=0 DO { out2(h2!gdeflist, h3!gdeflist)
                        gdeflist := h1!gdeflist
                      }
  RESULTIS TRUE // Successful return
}

LET trnext(next) BE
{ // Compile code to follow a command
  // next is >0, =0 or =-1
  
  IF next=0 RETURN // No code to compile.
  
  IF next>0 DO
  { out2(s_jump, next) // Compile a simple jump
    RETURN
  }
  
  // next must be =-1
  TEST proccontext=s_fnrn
  THEN { out2(s_ln, 0)
         out1(s_fnrn)
       }
  ELSE { out1(s_rtrn)
       }
}

LET trans(x, next) BE
// x       is the parse tree of the command to be translated
// next=-2 System error.
// next=-1 Compile the command and follow by code to return from
//         the current function or routine as specified by
//         retlab and proccontext.
// next>0  Compile the command followed by a JUMP next
// next=0  Compile x only

{ LET op, sfop, ff = ?, ?, FALSE
  LET sw = FALSE
  
  IF x=0 DO { trnext(next) // next>0 compile JUMP next.
                           // next<0 compile code equivalent to
			   //        LN 0; FNRN or RTRN.
                           // next=0 compile nothing.
              RETURN
	    }

  op := h1!x // op is the leading operator of
             // the command to translate.

  SWITCHON op INTO
  { DEFAULT: trnerr("System error in Trans, op = %s", opname(op))
             RETURN
 
    CASE s_let:
    { // x -> [s_let, defs, body, lm]
      LET prevcasecount = casecount
      LET prevdvece     = dvece
      LET prevssp, s1   = ssp, 0
      LET prevvecssp    = vecssp
      casecount := -1 // Disallow CASE and DEFAULT labels
                      // but ENDCASE is still be allowed.
      context, comline := x, h4!x

      declnames(h2!x)
      checkdistinct(prevdvece)
      vecssp, s1 := ssp, ssp
      ssp := prevssp
      context, comline := x, h4!x
      transdef(h2!x)
      UNLESS ssp=s1 DO trnerr("Lhs and rhs do not match")
      UNLESS ssp=vecssp DO { ssp := vecssp; out2(s_stack, ssp) }
      out1(s_store)
      decllabels(h3!x)
      trans(h3!x, next)
      vecssp := prevvecssp
      UNLESS ssp=prevssp DO out2(s_stack, prevssp)
      ssp := prevssp
      casecount := prevcasecount
      undeclare(prevdvece)
      RETURN
    }
 
    CASE s_static:
    CASE s_global:
    CASE s_manifest:
    { LET prevcasecount = casecount
      LET prevdvece = dvece
      LET prevssp   = ssp
      AND y, n = h2!x, 0
      LET prevk = 0 // The previous integer or floating point value
      LET prevt = 0 // =0, s_notflt or s_flt
         
      casecount := -1 // Disallow CASE and DEFAULT labels
                      // but ENDCASE may still be allowed
      context, comline := x, h4!x
 
      WHILE y DO
      { LET name = h3!y
        LET fop = op    // = s_static, s_global or s_manifest
        ff := FALSE     // ff will only be TRUE for static and manifest
                        // names with the FLT tag. If TRUE the value of
                        // the constant will be a floating point number.

        context, comline := y, h5!y

        // If the name is prefixed by FLT remove the prefix and
        // modify fop as follows
        //     s_static   -> s_fstatic
        //     s_manifest -> s_fmanifest
        // and s_global   -> s_fglobal

        IF h1!name=s_flt DO name, fop := h2!name, op | s_fltbit

        // If fop is s_fstatic or s_fmanifest the constant
        // expression in evaluated in an FLT context, in all
        // other cases it is evaluated in a non FLT context.

        IF fop=s_fstatic | fop=s_fmanifest DO ff := TRUE

        TEST h4!y
        THEN { n := evalconst(h4!y, ff)
             }
        ELSE { // The constant expression was not given so the
               // value is chosen as follows:
               // If there was no previous value the value is
               // 0 or 0.0.
               TEST prevt=0
               THEN { n := ff -> flt0, 0
                    }
               ELSE { IF fop=s_static  DO n := 0
                      IF fop=s_fstatic DO n := flt0

                      // for s_manifest  the value is one larger than the
                      //                 previous value is converted to
                      //                 integer, if necessary.

                      IF fop=s_manifest DO
                      { IF prevt=s_flt DO n := sys(Sys_flt, fl_fix, n)
                        n := n+1
                      }

                      // for s_fmanifest the value is 1.0 larger than the
                      //                 previous value is converted to
                      //                 floating point, if necessary.

                      IF fop=s_fmanifest DO
                      { IF prevt=s_notflt DO n := sys(Sys_flt, fl_float, n)
                        n := sys(Sys_flt, fl_add, n, flt1)
                      }

                      // For s_global and s_fglobal the value is a
                      // global number one larger than the previous one.

                      IF fop=s_global | fop=s_fglobal DO n := n + 1
                    }
             }

        // prevk is the previous value (integer or floating point).
        // prevt is s_flt if the previous value was floating point.
        //       it is s_notflt the previous value was an integer.

        prevk := n
        prevt := ff -> s_flt, s_notflt

        IF op=s_static DO
        { LET k = n
          n := genlab()      // n is now the label for the static variable
          out2(s_datalab, n)
          out2(s_itemn, k)
        }

        IF op=s_global UNLESS 0<=n<=65535 DO
        { trnerr("Global number %n too large for: %s*n", n, @h3!name)
	  n := 0
        }
        // n is a global number, a manifest value or a label for a
        // static variable.
        addname(name, fop, n, 0)
        IF xrefing DO xref(name,
                           (fop=s_global->"G:",
                            fop=s_fglobal->"FG:",
                            fop=s_static->"S:",
                            fop=s_fstatic->"FS:",
                            fop=s_manifest->"M:",
                            fop=s_fmanifest->"FM:",
                            "??:"),
                           n,
                           s_constdef
                          )
        y := h2!y
      }
 
      decllabels(h3!x)
      trans(h3!x, next)
      ssp := prevssp
      casecount := prevcasecount
      undeclare(prevdvece)
      RETURN
    }

    CASE s_matchc: // x -> [s_matchc, args, mlist, ln]
    CASE s_everyc: // x -> [s_everyc, args, mlist, ln]
    { // This has much in common with a SWITCHON command in that is
      // can select one or more of many alternatives and can use a
      // selection of simple commands to escape from match patterns
      // and match item commands or expressions. These commands are
      // EXIT, NEXT, BREAK, LOOP, ENDCASE and RESULTIS provided the
      // jump does not leave the current function or routine.
      // Of course RETURN is allowed.

      LET argpos = ssp // The position of the first argument of
                       // the MATCH or EVERY command.

      context, comline := x, h4!x

      patresultpos := 0 // Only non zero when matchcontext is s_everye
      
      // Load the MATCH or EVERY command arguments
      loadlist(h2!x)
      out1(s_store) // Ensure that the arguments stored are in memory

      // Translate the match items
      transmatchlist(op,     // The match context s_matchc or s_everyc
                     h3!x,   // mlist -> [matchitemc, plist, C, link, ln]
                     argpos, // Position of the first argument
		     next)   // Copy of the second argument of trans
		             // It will be >0, =0 or =-1 only.

      RETURN  // No need to call trnext(next) since this has
              // already been done by transmatchlist.
    }
     
    CASE s_assmul: // Assignment operators that might
    CASE s_assdiv: // be promoted to floating point.
    CASE s_assmod:
    CASE s_assadd:
    CASE s_asssub:
    CASE s_ass:
      // Note that simultaneous assignments have already been
      // replaced by sequences of simple assignments.
      // Convert op:= to #op:= if either operand has the FLT tag.
      UNLESS isflt(h2!x) | isflt(h3!x) DO
      { // Compile a non FLT assignment
        context, comline := x, h4!x
        op := assop2op(op)
        assign(h2!x, h3!x, FALSE, op)
        trnext(next)
        RETURN
      }

      // Promote op:= to #op:=
      op := cv2flt(op)
      h1!x := op
      // Fall through

    CASE s_assfmul: // The floating point assignment operators
    CASE s_assfdiv:
    CASE s_assfmod:
    CASE s_assfadd:
    CASE s_assfsub:
    CASE s_fass:
      context, comline := x, h4!x
      op := assop2op(op)
      assign(h2!x, h3!x, TRUE, op)
      trnext(next)
      RETURN

    CASE s_assvecap:  // All the other assignment operators
    CASE s_asslshift:
    CASE s_assrshift:
    CASE s_asslogand:
    CASE s_asslogor:
    CASE s_asseqv:
    CASE s_assxor:
      context, comline := x, h4!x
      op := assop2op(op)
      assign(h2!x, h3!x, FALSE, op)
      trnext(next)
      RETURN
 
    CASE s_rtap:
    { LET prevssp = ssp
      context, comline := x, h4!x
      ssp := ssp+savespacesize // Position of the first argument
      out2(s_stack, ssp)
      loadlist(h3!x) // Load arguments in non FLT mode
      load(h2!x, FALSE)
      out2(s_rtap, prevssp)
      ssp := prevssp
      trnext(next)
      RETURN
    }
 
    CASE s_goto:
      context, comline := x, h3!x
      load(h2!x, FALSE)
      out1(s_goto)
      ssp := ssp-1
      RETURN
 
    CASE s_colon:
      context, comline := x, h5!x
      out2(s_lab, h4!x)
      trans(h3!x, next)
      RETURN
 
    CASE s_unless: sw := TRUE
    CASE s_if:
      context, comline := x, h4!x

      // Optimise IF or UNLESS E followed by
      // BREAK, LOOP, ENDCASE, NEXT, EXIT or RETURN, if possible.

      SWITCHON h1!(h3!x) INTO // The leading operator of the body.
                              // Optimising commands like BREAK and EXIT.
      { DEFAULT: // No optimisation
          //TEST next>0
          //THEN { // If the condition is not equal to sw skip aroung the body. 
	  //       jumpcond(h2!x, ~sw, next)
          //       trans(h3!x, next)
          //     }
          //ELSE
	       { // Compile:            // IF B~=sw DO C
                 LET L = genlab()
                 jumpcond(h2!x, sw, L)  // If B=sw goto L
                 trans(h3!x, next)      //   C
                 out2(s_lab, L)         // L:
                 trnext(next)
               }
          RETURN

        CASE s_break: // In IF/UNLESS in trans
          condbreak(h2!x, ~sw, next)
          RETURN

        CASE s_loop: // In IF/UNLESS in trans
          condloop(h2!x, ~sw, next)
	  RETURN

        CASE s_endcase: // In IF/UNLESS in trans
          condendcase(h2!x, ~sw, next)
          RETURN

        CASE s_next: // In IF/UNLESS in trans
          condnext(h2!x, ~sw, next)
          RETURN

        CASE s_exit: // In IF/UNLESS in trans
          condexit(h2!x, ~sw, next)
          RETURN

        CASE s_return: // In IF/UNLESS in trans
          condreturn(h2!x, ~sw, next)
          RETURN
      }
      trnext(next)
      RETURN
 
    CASE s_test:
    { LET L, M = genlab(), 0
      context, comline := x, h5!x
      jumpcond(h2!x, FALSE, L)
         
      TEST next=0 THEN { M := genlab()
                         trans(h3!x, M)
		       }
                  ELSE { trans(h3!x, next)
		       }
                     
      out2(s_lab, L)
      trans(h4!x, next)
      IF M DO out2(s_lab, M)
      RETURN
    }
 
    CASE s_break: // Unconditional in trans
      context, comline := x, h2!x
      transbreak()
      RETURN

    CASE s_loop: // Unconditional in trans
      context, comline := x, h2!x
      transloop()
      RETURN

    CASE s_endcase: // Unconditional in trans
      context, comline := x, h2!x
      transendcase()
      RETURN

    CASE s_next: // Unconditional in trans
      context, comline := x, h2!x
      transnext()
      RETURN

    CASE s_exit: // Unconditional in trans
      context, comline := x, h2!x
      transexit()
      RETURN
 
    CASE s_return: // Unconditional in trans
      context, comline := x, h2!x
      transreturn()
      RETURN
 
    CASE s_skip:  // MR 05/4/06
      trnext(next)
      RETURN

    CASE s_finish:
      context, comline := x, h2!x
      out1(s_finish)
      RETURN
 
    CASE s_resultis:
      context, comline := x, h3!x
      IF resultlab=-2 DO trnerr("RESULTIS out of context")
      IF resultlab< 0 DO { fnbody(h2!x, FALSE); RETURN }
      IF resultlab=0 DO resultlab := genlab() // Probably not needed
      load(h2!x, FALSE)
      out2(s_res, resultlab)
      ssp := ssp - 1
      RETURN
 
    CASE s_while: sw := TRUE
    CASE s_until:
    { // If next>0, It is the label to jump to after the
      //            code for the command.
      // If next=-1 After the code for the command compile a return
      //            from the current procedure based on proccontext
      //            and retlab.
      // otherwise  compile nothing after the command.
      LET L = genlab()  // Label for start of the body
      LET prevbreaklab, prevlooplab = breaklab, looplab
      context, comline := x, h4!x
 
      breaklab := genlab() // Destination for BREAK as follows, ????
			   // =-2 BREAK is not legal here
                           // =0  Update breaklab with a new label
			   // >0  Compile BREAK as a jump to breaklab
			   // <-1 Compile BREAK as a return from a function
			   //     or routine based in proccontext and retlab.
      IF breaklab= 0 DO breaklab :=  genlab()
      IF breaklab=-1 DO breaklab := -genlab()
      
      looplab := 0         // This will be allocated if LOOP occurs.

      jumpcond(h2!x, ~sw, breaklab)
      
      out2(s_lab, L)    // Label the start of the body
      trans(h3!x, 0)    // Zero because the body will be followed by
                        // the conditional jump code

      context, comline := x, h4!x
      IF looplab DO out2(s_lab, looplab) // Only compiled if LOOP occurred.
      jumpcond(h2!x, sw, L)              // Compile the conditional jump.

      IF breaklab>0 DO out2(s_lab, breaklab) // A BREAK command
                                             // jumped to breaklab
      trnext(next) // Possibly compile a jump or a return from
                   // a function or routine.
      breaklab, looplab := prevbreaklab, prevlooplab
      RETURN
    }
 
    CASE s_repeatwhile: sw := TRUE
    CASE s_repeatuntil:
    { LET L = genlab()
      LET prevbreaklab, prevlooplab = breaklab, looplab
      context, comline := x, h4!x
      breaklab := genlab() // Cause BREAK to compile a jump or a return
                           // from the current function or routine.
                           // If next=0 breaklab will be given a newly
		           // allocated label.
		           // If next=-1 the code will depend on
		           // proccontext.
      looplab := genlab()  // Allocated by the first LOOP, if any.

      out2(s_lab, L)       // Label start of body
      trans(h2!x, 0)       // Zero because it is followed by the
                           // conditional jump
      // Compile the destination label for LOOP if necessary.
      IF looplab DO out2(s_lab, looplab)
      context, comline := x, h4!x
      jumpcond(h3!x, sw, L)

      // Compile the destination label for BREAK, if necessary.
      //IF breaklab>0 UNLESS breaklab=next DO out2(s_lab, breaklab)
      IF breaklab>0 DO out2(s_lab, breaklab)

      trnext(next) // Compile a jump, a return or nothing
      breaklab, looplab := prevbreaklab, prevlooplab
      RETURN
    }
 
    CASE s_repeat:
    { LET bl, ll = breaklab, looplab
      context, comline := x, h4!x
      breaklab := next     // This is >0, =0 or =-1
      looplab := genlab()  // The destinaion label for LOOP is
                           // always allocated.
      
      out2(s_lab, looplab)
      trans(h2!x, looplab) // Compile the loop

//IF hard DO
//{ sawritef("case repeat: next=%n nextlab=%n breaklab=%n*n",
//                     next, looplab, breaklab)
//  abort(3222)
//}

      // Compile the destination label for BREAK, if necessary.
      IF breaklab>0 & breaklab~=next |
         breaklab<-2 DO
      { out2(s_lab,  ABS breaklab)
        trnext(next) // Compile a jump, a return or nothing
      }
      
      IF breaklab=-1 DO
      { // Compile a return from the current function or routine
        TEST proccontext=s_rtrn
        THEN { out1(s_rtrn)
             }
        ELSE { out2(s_ln, 0)
               out1(s_fnrn)
             }
      }

      breaklab, looplab := bl, ll
      RETURN
    }
 
    CASE s_case:
    { LET l, k, p = genlab(), ?, caselist
      context, comline := x, h4!x
      k := evalconst(h2!x, FALSE)
      IF casecount<0 DO trnerr("CASE label out of context")
      WHILE p DO
      { IF h2!p=k DO trnerr("'CASE %n:' occurs twice", k)
        p := h1!p
      }
      caselist := newblk(caselist, k, l)
      casecount := casecount + 1
      //IF hard DO sawritef("s_case: casecont=%n k=%n l=%n*n", casecount, k, l)
      out2(s_lab, l)
      trans(h3!x, next)
      RETURN
    }
 
    CASE s_default:
      context, comline := x, h3!x
      IF casecount<0 DO trnerr("DEFAULT out of context")
      IF defaultlab DO trnerr("DEFAULT defined twice")
      defaultlab := genlab()
      out2(s_lab, defaultlab)
      trans(h2!x, next)
      RETURN
 
    CASE s_switchon:
      transswitch(x, next)
      RETURN
 
    CASE s_for:
      transfor(x, next)
      RETURN
 
    CASE s_seq:
      trans(h2!x, 0)
      x := h3!x
      LOOP
  }
} REPEAT

LET declnames(x) BE UNLESS x=0 SWITCHON h1!x INTO
  // x is the definition(s) following LET, so the leading operator is
  // one of s_vecdef, s_valdef, s_fndef, s_rtdef, s_patfndef, s_patrtdef
  // or s_and. This function adds names to the declaration vector.
{ DEFAULT:
    trnerr("Compiler error in Declnames, op=%s", opname(h1!x))
    RETURN
 
  CASE s_vecdef:
    context, comline := x, h4!x
    IF h1!(h2!x)=s_flt DO
    { trnerr("Vector name must not have the FLT tag")
      h2!x := h2!(h2!x) // Remove the FLT tag
    }
    decldyn(h2!x)
    RETURN
 
  CASE s_valdef:
    context, comline := x, h4!x
    decldyn(h2!x)
    RETURN
 
  CASE s_rtdef:  // x -> [ rtdef, name, namelist, C, entrylab, ln ]
  CASE s_fndef:  //   |  [ fndef, name, namelist, E, entrylab, ln ]
    context, comline := x, h6!x
    IF h1!(h2!x)=s_flt DO
    { trnerr("Procdure names must not have the FLT tag")
      h2!x := h2!(h2!x) // Remove the FLT tag
    }
    h5!x := genlab()     // The entry label.
    declstat(h2!x, h5!x) // Declare the procedure name.
    RETURN
 
  CASE s_patrtdef: // x -> [ patrtdef, name, matchlist, entrylab, ln ]
  CASE s_patfndef: //   |  [ patfndef, name, matchlist, entrylab, ln ]
    context, comline := x, h5!x
    IF h1!(h2!x)=s_flt DO
    { trnerr("Function name must not have the FLT tag")
      h2!x := h2!(h2!x) // Remove the FLT tag
    }
    h4!x := genlab()     // Choose the entry point label number
    declstat(h2!x, h4!x) // Declare the patfn or patrt name.
    RETURN
 
  CASE s_and:
    declnames(h2!x)
    declnames(h3!x)
}

AND decldyn(x) BE UNLESS x=0 DO 
{ // x is a list of names with possible FLT tags.
  // They are added to the declaration vector with kind
  // s_local or s_flocal.
  LET k = s_local

  IF h1!x=s_flt DO
  { k := s_flocal     // x -> [s_flt, [s_name.chain,<caracters>]
    x := h2!x
  }

  IF h1!x=s_name DO
  { addname(x, k, ssp, 0)
    IF xrefing DO
    { xref(x,
           (k=s_local -> "P:", "FP:"),
           ssp, s_local)
    }
    ssp := ssp + 1
    RETURN
  }
  
  IF h1!x=s_comma DO
  { decldyn(h2!x)
    decldyn(h3!x)
    RETURN
  }
 
  trnerr("Compiler error in Decldyn")
}

AND declstat(x, lab) BE
{ LET c = cellwithname(x)
  LET fk = h2!c
  LET k = fk & s_fltmask 
  TEST k=s_global
  THEN { LET gn = h3!c
         gdeflist := newblk(gdeflist, gn, lab)
         gdefcount := gdefcount + 1
         addname(x, s_global, gn, 0)
         IF xrefing DO
           xref(x,
                (fk=s_fglobal -> "FG:", "G:"),
                gn, h1!context)
                IF gdefsing DO
                { writef("G%i3 = %s*n", gn, @h3!x)
                  //abort(1000)
                }
      }
 ELSE { addname(x, s_label, lab, 0)
        IF xrefing DO
          xref(x,
               (fk=s_flocal -> "FF:", "F:"),
               lab, h1!context)
      }
}
 
AND decllabels(x) BE
{ // Declare the labels in the body globale, static, manifest,
  // let bolcks and the body of rtdef, patrtdef and valof blocks,
  // and for commands.
  LET e = dvece
  scanlabels(x)
  checkdistinct(e)
}
 
AND checkdistinct(p) BE
{ LET lim = dvece - 4
  FOR q = p TO lim-4 BY 4 DO
  { LET n = h1!q
    FOR c = q+4 TO lim BY 4 DO
        IF h1!c=n DO trnerr("Name %s defined twice", @h3!n)
  }
}
 
AND addname(name, k, a, path) BE
{ LET p = dvece + 4
  IF p>dvect DO trnerr("More workspace needed")
  h1!dvece, h2!dvece, h3!dvece, h4!dvece := name, k, a, path
  h2!name := dvece // Remember the declaration
  dvece := p
  //writef("addname: name cell at=%n %s k=%s a=%n path=%x8*n",
  //        dvece-4, @h3!name, opname(k), a, path)
}
 
AND undeclare(e) BE 
{ FOR t = e TO dvece-4 BY 4 DO
  { LET name = h1!t
    h2!name := 0   // Forget the declaration of this name.
  }
  dvece := e
}

AND cellwithname(n) = VALOF
{ // n is a name node not prefixed by FLT.
  LET t = h2!n
  IF t RESULTIS t  // It has been looked up before
  t := dvece
  t := t - 4 REPEATUNTIL h1!t=n | h1!t=0
  h2!n := t  // Associate the name with declaration item
  RESULTIS t
}
 
AND scanlabels(x) BE UNLESS x=0 SWITCHON h1!x INTO
 
{ CASE s_colon:   context, comline := x, h5!x
                  h4!x := genlab()
                  declstat(h2!x, h4!x)
 
  CASE s_if: CASE s_unless: CASE s_while: CASE s_until:
  CASE s_switchon: CASE s_case:
                  scanlabels(h3!x)
                  RETURN
 
  CASE s_seq:     scanlabels(h3!x)
 
  CASE s_repeat:
  CASE s_repeatwhile:
  CASE s_repeatuntil:
  CASE s_default: scanlabels(h2!x)
                  RETURN
 
  CASE s_test:    scanlabels(h3!x)
                  scanlabels(h4!x)
  DEFAULT:        RETURN
}
 
AND transdef(x) BE
{ LET ctxt, ln = context, comline
  transdyndefs(x)
  context, comline := ctxt, ln
  IF statdefs(x) DO { // The declarations in x include at least one
                      // function or routine definition.
                      LET L, s = 0, ssp
                      IF procname DO
		      { // e are not compiling outer level declarations
		        L := genlab()
		        out2(s_jump, L)
                      }
                      transstatdefs(x) // Translate the function and
		                       // routine definitions in x.
                      ssp := s
                      out2(s_stack, ssp)
                      IF L DO out2(s_lab, L)
                    }
  context, comline := ctxt, ln
}
 
 
AND transdyndefs(x) BE SWITCHON h1!x INTO
{ CASE s_and:    transdyndefs(h2!x)
                 transdyndefs(h3!x)
                 RETURN
 
  CASE s_vecdef: context, comline := x, h4!x
                 out2(s_llp, vecssp)
                 ssp := ssp + 1
                 vecssp := vecssp + 1 + evalconst(h3!x, FALSE)
                 RETURN
 
  CASE s_valdef: // Compile initialisation code for declaration
                 // N = E  or  FLT N = E
                 context, comline := h3!x, h4!x
                 load(h3!x, h1!(h2!x)=s_flt -> TRUE, FALSE)
  DEFAULT:       RETURN
}
 
AND transstatdefs(x) BE SWITCHON h1!x INTO
{ // Translate s_rtdef, s_fndef, s_patrtdef and s_patfndef
  // Ignore the other declarations.
  
  DEFAULT:     RETURN

  CASE s_and:  transstatdefs(h2!x)
               transstatdefs(h3!x)
               RETURN

  // In transstatdef
  CASE s_rtdef:  // x -> [ rtdef, name, namelist, C, entrylab, ln ]
  CASE s_fndef:  // x -> [ fndef, name, namelist, E, entrylab, ln ]
  { LET prevdvece    = dvece     // These must be restored at the end 
    LET prevdvecp    = dvecp
    LET prevprocname = procname

    // EXIT, NEXT, BREAK, LOOP, RESULTIS and ENDCASE cannot jump
    // out of a function or routine, ie they must leave the P
    // pointer unchanged.
    AND prevexitlab    = exitlab
    AND prevnextlab    = nextlab
    AND prevbreaklab   = breaklab
    AND prevlooplab    = looplab
    AND prevresultlab  = resultlab
    AND prevendcaselab = endcaselab
    
    AND prevcaselist   = caselist
    AND prevcasecount  = casecount

    AND prevmatchcontext  = matchcontext
    AND prevproccontext   = proccontext

    AND argpos = savespacesize
    AND body = h4!x

    matchcontext := 0  // Not in a match construct.

    TEST h1!x=s_rtdef
    THEN proccontext := s_rtrn
    ELSE proccontext := s_fnrn
    
    procname := h2!x // The FLT tag if any will have been 
                     // removed by declstat.
	  	     // Note procname points to a name node
		     // when compiling a procedure.
    
    context, comline := x, h6!x

    out2(s_entry, h5!x)      // The label allocated for the entry point.
    outstring(@h3!procname)

    dvecp := dvece           // To disallow dynamic free variables.
    
    
    ssp := savespacesize
    decldyn(h3!x)            // Declare the formal parameters.
    checkdistinct(prevdvece) // Check that they are distinct.

    out2(s_save, ssp)        // ssp is savespacesize plus the number
                             // of formal parameters.

    context, comline := body, h6!x

    nextlab               := -2     // NEXT illegal
    exitlab               := -2     // EXIT illegal
    breaklab,  looplab    := -2, -2 // BREAK and LOOP illegal
    resultlab, endcaselab := -2, -2 // RESULTIS and ENDCASE illegal
    casecount             := -1     // CASE and DEFAULT illegal

    caselist :=  0  // This is not really necessary since it is
                    // set to zero by transswitch.
    
    TEST h1!x=s_rtdef
    THEN { LET e1 = dvece
           proccontext := s_rtrn
           decllabels(body)
           trans(body, -1)     // Compile body followed by RTRN
           undeclare(e1)       // Undeclare the labels
         }
    ELSE { proccontext := s_fnrn
           fnbody(body, FALSE) // Compile the body expression in
         }                     // non FLT mode followed by FNRN.
//sawritef("At end of a procedure definition of %s*n", @h3!procname)

    TEST proccontext=s_rtrn
    THEN out1(s_rtrn)
    ELSE out3(s_ln, 0, s_fnrn)
    
    out1(s_endproc)

    exitlab               := prevexitlab
    nextlab               := prevnextlab
    breaklab,  looplab    := prevbreaklab,  prevlooplab
    resultlab, endcaselab := prevresultlab, prevendcaselab
    caselist,  casecount  := prevcaselist,  prevcasecount

    proccontext  := prevproccontext
    matchcontext := prevmatchcontext
    
    procname := prevprocname
    dvecp    := prevdvecp
    undeclare(prevdvece)    // Undeclare the formal parameters
    RETURN
  }
 
  // In trasstatdef
  CASE s_patrtdef: // x -> [ patrtdef, name, mlist, entrylab, ln ]
  CASE s_patfndef: //   |  [ patfndef, name, mlist, entrylab, ln ]
//  { // Note that dvece is saved and restored when translating match items.
  { LET prevdvece    = dvece     // These must be restored at the end
  LET xxx = 123
    LET prevdvecp    = dvecp
    LET prevprocname = procname
    AND mlist  = h3!x
    AND argpos = savespacesize

    procname := h2!x // The FLT tag. if any. will have been 
                     // removed by declstat.

    context, comline := x, h5!x

    out2(s_entry, h4!x)      // The label allocated for the entry point.
    outstring(@h3!procname)

    dvecp := dvece           // To diasallow dynamic free variables.
    
    // The arguments will have been laid out in the stack starting
    // at argpos by the code to call this function or routine.

    // Note that SAVE will be compiled by transmatchlist when it
    // knows how many arguments are inspected by the pattern items.

    // Translate all the match items
    transmatchlist(h1!x,   // The context is s_patfndef or s_patrtdef
                   mlist,  // mlist -> [matchiteme, plist, E, link, ln]
                           //       |  [matchitemc, plist, C, link, ln]
                   argpos, // Position of the first match argument
		   0)      // Not used in this context

    out1(s_endproc)

    dvecp    := prevdvecp
    procname := prevprocname
  }
}

AND arglength(mlist) = VALOF
{ // mlist ->  [ matchiteme, plist, E, link, ln ]
  //       |   [ matchitemc, plist, C, link, ln ]
  // link points to the next match item, if any

  // Return the maximum number of arguments discovered in any
  // match item in mlist.
  LET res = 0

  WHILE mlist DO
  { IF h2!mlist DO
    { LET len = patarglength(h2!mlist)
      IF res < len DO res := len
    }
    mlist := h4!mlist
  }

  RESULTIS res
}

AND patarglength(plist) = VALOF
{ // plist => 0                           => zero
  //       |  [ comma. plist, plist ]     => sum of the two plists
  //       |  all othe pat nodes          => length 1
  IF plist=0  RESULTIS 0
  IF h1!plist=s_comma RESULTIS patarglength(h2!plist) + patarglength(h3!plist)
  RESULTIS 1
}

AND transbreak() BE
{ // Compile the BREAK command.

  // breaklab=-2 when BREAK is illegal.
  // breaklab=-1   Compile a return from a procedure using
  //               RTRN or LN 0; FNRN.
  // breaklab= 0   Update breaklab with a new lable and compile a jump.
  // breaklab> 0   Compile a jump to breaklab.

  IF breaklab =-2 DO
  { trnerr("BREAK out of context")
    RETURN
  }

  IF breaklab<0 DO
  { TEST proccontext=s_rtrn
    THEN out1(s_rtrn)
    ELSE out3(s_ln, 0, s_fnrn)
    RETURN
  }

  // breaklab is positive
  IF breaklab=0 DO breaklab := genlab()
  out2(s_jump, breaklab)
  RETURN
}

AND condbreak(x, b, next) BE
{ // Compile a conditional execution of BREAK.
  // Compile code to execute BREAK if x yields the truth value b,
  // otherwise compile code specified by next, as follows.
  // next>0  Compile JUMP next
  // next=0  Compile nothing
  // next<0  Compile RTRN or LN 0; FNRN

  LET L = ?
  
  IF breaklab<0 DO
  { IF next>0 DO
    { jumpcond(x, ~b, next)
      transbreak()
      RETURN
    }
    
    L := genlab()
    
    IF next=0 DO
    { jumpcond(x, ~b, L)
      transbreak()
      out2(s_lab, L)
      RETURN
    }
    
    // Both breaklab and next are negative, so compile x in
    // truth value mode discarding the result followed by
    // RTRN or LN 0; FNRN.
    jumpcond(x, ~b, L)
    out2(s_lab, L)
    transbreak()
    RETURN
  }

  // breaklab is positive, so allocate breaklab if necessary
  // and compile a conditional jmp to breaklab.
  
  UNLESS breaklab DO breaklab := genlab()
  jumpcond(x, b, breaklab)
}

AND transloop() BE
{ // Compile the LOOP command.

  // looplab=-2   LOOP is illegal.
  // looplab=-1   Compile a return from a procedure using
  //              RTRN or LN 0; FNRN.
  // looplab= 0   Update looplab with a new label and compile a jump.
  // looplab> 0   Compile a jump to looplab.

  IF looplab =-2 DO
  { trnerr("LOOP out of context")
    RETURN
  }

  IF looplab<0 DO
  { TEST proccontext=s_rtrn
    THEN out1(s_rtrn)
    ELSE out3(s_ln, 0, s_fnrn)
    RETURN
  }

  // looplab is positive
  IF looplab=0 DO looplab := genlab()
  out2(s_jump, looplab)
  RETURN
}

AND condloop(x, b, next) BE
{ // Compile a conditional execution of LOOP.
  // Compile code to execute LOOP if x yields the truth value b,
  // otherwise compile code specified by next, as follows.
  // next>0  Compile JUMP next
  // next=0  Compile nothing
  // next<0  Compile RTRN or LN 0; FNRN

  LET L = ?
  
  IF looplab<0 DO
  { IF next>0 DO
    { jumpcond(x, ~b, next)
      transloop()
      RETURN
    }
    
    L := genlab()
    
    IF next=0 DO
    { jumpcond(x, ~b, L)
      transloop()
      out2(s_lab, L)
      RETURN
    }
    
    // Both looplab and next are negative, so compile x in
    // truth value mode discarding the result followed by
    // RTRN or LN 0; FNRN.
    jumpcond(x, ~b, L)
    out2(s_lab, L)
    transloop()
    RETURN
  }

  // looplab is positive, so allocate looplab if necessary
  // and compile a conditional jmp to looplab.
  
  UNLESS looplab DO looplab := genlab()
  jumpcond(x, b, looplab)
}

AND transendcase() BE
{ // Compile the ENDCASE command.

  // endcaselab=-2   ENDCASE is illegal.
  // endcaselab=-1   Compile a return from a procedure using
  //              RTRN or LN 0; FNRN.
  // endcaselab= 0   Update endcaselab with a new label and compile a jump.
  // endcaselab> 0   Compile a jump to endcaselab.

  IF endcaselab =-2 DO
  { trnerr("ENDCASE out of context")
    RETURN
  }

  IF endcaselab<0 DO
  { TEST proccontext=s_rtrn
    THEN out1(s_rtrn)
    ELSE out3(s_ln, 0, s_fnrn)
    RETURN
  }

  // endcaselab is positive
  IF endcaselab=0 DO endcaselab := genlab()
  out2(s_jump, endcaselab)
  RETURN
}

AND condendcase(x, b, next) BE
{ // Compile a conditional execution of ENDCASE.
  // Compile code to execute ENDCASE if x yields the truth value b,
  // otherwise compile code specified by next, as follows.
  // next>0  Compile JUMP next
  // next=0  Compile nothing
  // next<0  Compile RTRN or LN 0; FNRN

  LET L = ?
  
  IF endcaselab<0 DO
  { IF next>0 DO
    { jumpcond(x, ~b, next)
      transendcase()
      RETURN
    }
    
    L := genlab()
    
    IF next=0 DO
    { jumpcond(x, ~b, L)
      transendcase()
      out2(s_lab, L)
      RETURN
    }
    
    // Both endcaselab and next are negative, so compile x in
    // truth value mode discarding the result followed by
    // RTRN or LN 0; FNRN.
    jumpcond(x, ~b, L)
    out2(s_lab, L)
    transendcase()
    RETURN
  }

  // endcaselab is positive, so allocate endcaselab if necessary
  // and compile a conditional jmp to endcaselab.
  
  UNLESS endcaselab DO endcaselab := genlab()
  jumpcond(x, b, endcaselab)
}

AND transnext() BE
{ // Compile the NEXT command.

  // nextlab=-2   NEXT is illegal.
  // nextlab=-1   Compile a return from a procedure using
  //              RTRN or LN 0; FNRN.
  // nextlab= 0   Update nextlab with a new label and compile a jump.
  // nextlab> 0   Compile a jump to nextlab.

  IF nextlab =-2 DO
  { trnerr("NEXT out of context")
    RETURN
  }

  IF nextlab<0 DO
  { TEST proccontext=s_rtrn
    THEN out1(s_rtrn)
    ELSE out3(s_ln, 0, s_fnrn)
    RETURN
  }

  // nextlab is positive
  IF nextlab=0 DO nextlab := genlab()
  out2(s_jump, nextlab)
  RETURN
}

AND condnext(x, b, next) BE
{ // Compile a conditional execution of NEXT.
  // Compile code to execute NEXT if x yields the truth value b,
  // otherwise compile code specified by next, as follows.
  // next>0  Compile JUMP next
  // next=0  Compile nothing
  // next<0  Compile RTRN or LN 0; FNRN

  LET L = ?
  
  IF nextlab<0 DO
  { IF next>0 DO
    { jumpcond(x, ~b, next)
      transnext()
      RETURN
    }
    
    L := genlab()
    
    IF next=0 DO
    { jumpcond(x, ~b, L)
      transnext()
      out2(s_lab, L)
      RETURN
    }
    
    // Both nextlab and next are negative, so compile x in
    // truth value mode discarding the result followed by
    // RTRN or LN 0; FNRN.
    jumpcond(x, ~b, L)
    out2(s_lab, L)
    transnext()
    RETURN
  }

  // nextlab is positive, so allocate nextlab if necessary
  // and compile a conditional jmp to nextlab.
  
  UNLESS nextlab DO nextlab := genlab()
  jumpcond(x, b, endcaselab)
}

AND transexit() BE
{ // Compile the EXIT command.

  // If matchcontext=0 or exitlab=-2, EXIT is illegal.

  // If matchcontext is s(patrtdef or s_patfndef, compile
  //    LN 0; FNRN
  // If matchcontext is s_everye, compile LP patresultpos
  // otherwiise compile lN 0
  // then compile FNRN or  J exitlab based on exitlab.

  SWITCHON matchcontext INTO
  { DEFAULT:
      trnerr("EXIT out of context")
      RETURN

    CASE s_patrtdef:
    CASE s_patfndef:
      out3(s_ln, 0, s_fnrn)
      RETURN

    CASE s_matchc:
    CASE s_matche:
    CASE s_everyc:
    CASE s_everye:
      TEST matchcontext=s_everye
      THEN out2(s_lp, patresultpos)
      ELSE out2(s_ln, 0)
      TEST exitlab<0
      THEN { out1(s_fnrn)
           }
      ELSE { IF exitlab=0 DO exitlab := genlab()
             out2(s_jump, exitlab)
	   }
      RETURN
  }
}

AND condexit(x, b, next) BE
{ // Compile a conditional execution of EXIT.
  // Compile code to execute EXIT if x yields the truth value b,
  // otherwise compile code specified by next, as follows.
  // next>0  Compile JUMP next
  // next=0  Compile nothing
  // next<0  Compile RTRN or LN 0; FNRN

  LET L = ?
  
  IF exitlab<0 DO
  { IF next>0 DO
    { jumpcond(x, ~b, next)
      transexit()
      RETURN
    }
    
    L := genlab()
    
    IF next=0 DO
    { jumpcond(x, ~b, L)
      transexit()
      out2(s_lab, L)
      RETURN
    }
    
    // Both exitlab and next are negative, so compile x in
    // truth value mode discarding the result followed by
    // RTRN or LN 0; FNRN.
    jumpcond(x, ~b, L)
    out2(s_lab, L)
    transexit()
    RETURN
  }

  // exitlab is positive, so allocate exitlab if necessary
  // and compile a conditional jmp to exitlab.
  
  UNLESS exitlab DO exitlab := genlab()
  jumpcond(x, b, endcaselab)
}

AND transreturn() BE
{ TEST proccontext=s_rtrn
  THEN out1(s_rtrn)
  ELSE out3(s_ln, 0, s_fnrn)
}

AND condreturn(x, b, next) BE
{ // Compile a conditional execution of RETURN.
  // Compile code to execute RETURN if x yields the truth value b,
  // otherwise compile code specified by next, as follows.
  // next>0  Compile JUMP next
  // next=0  Compile nothing
  // next<0  Compile RTRN or LN 0; FNRN

  LET L = ?
  
  IF next>0 DO
  { jumpcond(x, ~b, next)
    transreturn()
    RETURN
  }
    
  L := genlab()
    
  IF next=0 DO
  { jumpcond(x, ~b, L)
    transreturn()
    out2(s_lab, L)
    RETURN
  }
    
  // next is negative, so compile x in truth value mode discarding
  // the result followed by RTRN or LN 0; FNRN.
  jumpcond(x, ~b, L)
  out2(s_lab, L)
  transreturn()
}

AND transmatchlist(mcontext, mlist, argpos, next) BE
{ // This function is only used to compile a match list in
  // one of the six contexts specified by mcontext namely:
  //   s_patfndef, s_patrtdef,
  //   s_matche,   s_matchc,
  //   s_everye or s_everyc

  // mlist points to the first match item which is
  // either  [ matchiteme, plist, E, link, ln ]
  //     or  [ matchitemc, plist, C, link, ln ]

  // plist is the pattern i the match item.
  // link points to the next match item in the match list, if any.

  // argpos is the stack position of the first argument, if any.
  // These arguments are already laid out on the the stack.
  
  // If mcontext is s_everye the location on the stack at position
  // patresultpos (=argpos-1) is used to hold the accumulated result.
  // patresultpos is otherwise zero.
  
  // next> 0  Compile a jump to label next after compiling the match list.
  // next= 0  Compile nothing after compiling the match list.
  // next=-1  After compiling the match construct compile code to return
  //          from the current procedure. This will typically be
  //          LN 0; FNRN or LP patresultpos; FNRN.

  SWITCHON mcontext INTO
  { // All possible match contexts
    DEFAULT:
      trnerr("SYSTEM ERROR in transmatchlist, op=%n", mcontext)
      abort(999)
      RETURN

    CASE s_patfndef:  // In transmatchlist
    CASE s_patrtdef:
    { LET prevmatchcontext  = matchcontext
      LET prevnextmatchitem = nextmatchitem

      LET prevproccontext   = proccontext
      
      LET prevssp        = ssp
      LET prevexitlab    = exitlab    // These labels must be preserved.
      LET prevnextlab    = nextlab    //
      
      LET prevcasecount  = casecount  // CASE and DEFAULT must belong to
                                      // a SWITCHON inside this procedure.

      LET prevbreaklab   = breaklab   // These labels must be preserved.
      LET prevlooplab    = looplab
      LET prevendcaselab = endcaselab
  
      LET prevresultlab  = resultlab  // RESULTIS must belong to a VALOF
                                      // inside this procedure.
      
      LET argcount = arglength(mlist) // The number of arguments
                                      // inspected by the match items

      // The number of actual arguments supplied may be more or less
      // than the number inspected by the match items.

      LET prevdvecp = dvecp // Since dvecp will be set to dvece to
                            // disallow dynamic free varables.
      dvecp := dvece

      // The global matchcontext affects the translation of various
      // commands such as NEXT and EXIT.

      matchcontext := mcontext 
      proccontext := matchcontext=s_patrtdef -> s_rtrn, s_fnrn
      

      breaklab,     looplab := -2, -2 // BREAK, LOOP, ENDCASE and
      endcaselab, resultlab := -2, -2 // RESULTIS are not allowed
                                      // since they would jump out
				      // of the current procedure.

      exitlab := 0    // EXIT is legal but not yet used. If used
                      // the code LAB exitlab; LN 0; FNRN will be
		      // compiled at the end of the match list.

      casecount := -1 // CASE and DEFAULT are illegal
                      // but ENDCASE may be allowed.

      // The translation of each match item starts with ssp set to
      // position of the last argument inspected by any of the match
      // items. This ensures the evaluation of the patterns and bodies
      // of match items do not corrupt arguments that may be inspected
      // later.

      ssp := argpos + argcount
      out2(s_save, ssp)  // Save the return link and set ssp

      // Translate each match item.
      WHILE mlist DO
      { // mlist -> [ matchiteme, plist, E, link, ln ]
        // or    -> [ matchitemc, plist, C, link, ln ]
	
        LET pattern, body = h2!mlist, h3!mlist

        LET prevdvece = dvece // Since this pattern may include the
                              // declaration of pattern variables.
	
        nextmatchitem := h4!mlist

        nextlab := 0 // This is only allocated if needed.

        context, comline := mlist, h5!mlist
        ssp := argpos + argcount

        // All match items start with ssp set to just beyond the
        // last match argument.
        out2(s_stack, ssp)
    
        declpatnames(pattern,  // The match item pattern
                     argpos,   // Position relative to P of the
                               // first argument
                     0,        // The current square bracket
                               // depth. The arguments are at
                               // depth zero.
		     0)        // Packed set of up to four 8-bit
		               // path offsets.

        checkdistinct(prevdvece) // The pattern names must be
	                         // distinct.

        ssp := argpos+argcount
	
        transpattern(pattern,
                     argpos,      // Argument position relative to P
                     0,           // The indirection depth
		     0,           // The indirection path
		     FALSE,       // If this pattern fails to match
		     0)           // the destination is specified by
		                  // matchcontext and other variables.

        // The current pattern was matched successfully.

        TEST matchcontext=s_patfndef
        THEN { fnbody(body, FALSE) // Compile body in non FLT mode
             }                     // followed by FNRN.
	ELSE { LET p = dvecp
	       decllabels(body)
               trans(body, -1)     // Compile body followed by a
	                           // routine return
               undeclare(p)        // Undeclare the labels.
	     }
	     
        undeclare(prevdvece)  // Undeclare the pattern variables
                              // declared in this match item.
                              // This resets dvece to prevdvece.

        IF nextlab DO // Compile a LAB nextlab, if needed.
	{ out2(s_lab, nextlab)
          nextlab := 0
	}
	
        // Translate the next match item, if any.
        mlist := nextmatchitem
      }

      // Compile code at the end of a pattern procedure.
      
      ssp := argpos
      out2(s_stack, ssp)   // Reset ssp

      // All match items have been compiled and at this point in
      // the code they have all failed.
      
      // Compile a LAB exitlab, if needed.
      IF exitlab DO out2(s_lab, exitlab)

      // Compile a LAB retlab, if needed.
      //IF retlab DO out2(s_lab, retlab)

      // All match items have failed or EXIT called so return
      // the result of zero.
      out2(s_ln, 0)
      out1(s_fnrn)

      matchcontext  := prevmatchcontext
      nextmatchitem := prevnextmatchitem
      proccontext   := prevproccontext
      
      //retlab        := prevretlab
      
      exitlab       := prevexitlab
      nextlab       := prevnextlab
      casecount     := prevcasecount

      breaklab      := prevbreaklab
      looplab       := prevlooplab
      endcaselab    := prevendcaselab
  
      resultlab     := prevresultlab
      ssp           := prevssp
      RETURN
    }



    CASE s_matchc:  // In transmatchlist
    CASE s_matche:
    CASE s_everyc:
    CASE s_everye:
    { LET prevmatchcontext  = matchcontext
      LET prevnextmatchitem = nextmatchitem

      LET prevexitlab    = exitlab    // These labels must be preserved.
      LET prevnextlab    = nextlab

      LET prevcasecount  = casecount  // CASE and DEFAULT must belong to
                                      // a SWITCHON inside this procedure.

      LET prevpatresultpos = patresultpos // Only used in everye

      LET argcount = arglength(mlist) // The number of arguments
                                      // inspected by the match items

      // The number of actual arguments supplied may be more or less
      // than the number inspected by the match items.

      // The global matchcontext affects the translation of various
      // commands such as NEXT and EXIT.

      matchcontext  := mcontext

      exitlab := 0    // EXIT is legal but not yet used. If used
                      // appropriate code will be compiled at the
		      // end of the match list.

      casecount := -1 // CASE and DEFAULT are illegal
                      // but ENDCASE may be allowed.

      // The translation of each match item starts with ssp set to
      // position of the last argument inspected by any of the match
      // items. This ensures the evaluation of the patterns and bodies
      // of match items do not corrupt arguments that may be inspected
      // later.

      // Translate each match item.      
      WHILE mlist DO
      { // mlist -> [ matchiteme, plist, E, link, ln ]
        // or    -> [ matchitemc, plist, C, link, ln ]

        LET pattern, body = h2!mlist, h3!mlist

        LET prevdvece = dvece // Since this pattern may include the
                              // declaration of pattern variables.
	
        nextmatchitem := h4!mlist

        nextlab := 0 // Only allocated if needed. It will not be allocated
	             // if, for instance, the pattern is query and the match
		     // list has a single item not containing NEXT.
      
        context, comline := mlist, h5!mlist
        ssp := argpos + argcount

        // All match items start with ssp set to just beyond the
        // last match argument.
        out2(s_stack, ssp)

        declpatnames(pattern,  // The match item pattern
                     argpos,   // Position relative to P of the
                               // first match argument
                     0,        // The current square bracket
                               // depth. The arguments are at
                               // depth zero.
		     0)        // Packed set of up to four 8-bit
		               // path offsets.

        checkdistinct(prevdvece) // The pattern names must be
	                         // distinct.

        ssp := argpos+argcount
        out2(s_stack, ssp)
//abort(3218)	
        transpattern(pattern,
                     argpos,      // Argument position relative to P
                     0,           // The indirection depth
		     0,           // The indirection path
		     FALSE,       // If this pattern fails to match
		     0)           // the destination is specified by
		                  // matchcontext and other variables.
        ssp := argpos+argcount
        out2(s_stack, ssp)

        // The translate the body of this matchitem.

outcomment("Code for body of a %s, *
           *nextlab=%n exitlab=%n next=%n",
           opname(matchcontext), nextlab, exitlab, next)

        SWITCHON matchcontext INTO
	{ DEFAULT:
	    trnerr("SYSTEM ERROR in transmatchlist")
	    ENDCASE

          CASE s_matchc:
	  { LET predvece = dvece
            decllabels(body)
	    // next is >0   The lable to jump to after executing the body
	    //         =0   If not the last match item allocate exitlab
	    //              if necessary and call trans(body, exitlab).
	    //              If the last match item call trans(body, 0)
	    //         <0   Return from the current procedure after executing
	    //              the body.
	    
	    TEST next
            THEN trans(body, next)                 // next>0 or next<0
	    ELSE TEST nextmatchitem                // next=0
                 THEN { // Not the last match item
		        UNLESS exitlab DO exitlab := genlab()
	                trans(body, exitlab)
                      }
	         ELSE { // The last match item
		        trans(body, 0)
	              }
		 
            undeclare(prevdvece)  // Undeclare the labels.
            ENDCASE
          }

          CASE s_matche:
            load(body, FALSE) // Load the body expression in non FLT mode.
	    TEST next<0
	    THEN { out1(s_fnrn)
	           ssp := ssp-1
	         }
	    ELSE { UNLESS exitlab DO exitlab := genlab()
	           out2(s_res, exitlab)
		   ssp := ssp-1
		 }
	    ENDCASE

          CASE s_everyc:
          { LET prevdvece = dvece
	    decllabels(body)

            TEST next
	    THEN { trans(body, next)  // next>0 or next<0
	         }
	    ELSE { UNLESS nextlab DO nextlab := genlab() // next=0
		   trans(body, nextlab)
	         }

            undeclare(prevdvece)  // Undeclare the labels.
	    ENDCASE
          }

         CASE s_everye:
            load(body, FALSE)
            out2(s_lp, patresultpos)
	    out1(s_add)
	    out2(s_sp, patresultpos)
            ENDCASE
	}

outcomment("Code for nextlab after body in a %s, *
           *nextlab=%n exitlab=%n next=%n",
            opname(matchcontext), nextlab, exitlab, next)

        IF nextlab DO
	{ out2(s_lab, nextlab) // Compile the label if necessary
	  nextlab := 0
	}

        undeclare(prevdvece)   // Undeclare the pattern variables
                               // resetting dvece.
	
        mlist := nextmatchitem
      }

      // All match items have been compiled.
      // Compile code after the last match item.

outcomment("Code at end of %s, nextlab=%n exitlab=%n next=%n",
            opname(matchcontext), nextlab, exitlab, next)

      SWITCHON matchcontext INTO
      { DEFAULT:
          trnerr("SYSTEM ERROR in transmatchlist")
          ENDCASE

        CASE s_matchc:
          ssp := argpos
          out2(s_stack, ssp)

          IF exitlab DO out2(s_lab, exitlab)
   
          IF next>0 DO out2(s_jump, next)
          IF next<0 TEST proccontext=s_rtrn
	            THEN out1(s_rtrn)
		    ELSE out3(s_ln, 0, s_fnrn)
          ENDCASE

        CASE s_matche:
          ssp := argpos
          out2(s_stack, ssp)

          UNLESS exitlab DO exitlab := genlab()
          out2(s_ln, 0)        // All match items failed.
          out2(s_res, exitlab)
	  
          out2(s_lab, exitlab) // Possibly reached from the body of
	                       // a successful match item.
          out2(s_rstack, ssp)
	  ssp := ssp+1
          IF next>0 DO out2(s_jump, next)
          IF next<0 DO out1(s_fnrn)
          ENDCASE

        CASE s_everyc:
          ssp := argpos
          out2(s_stack, ssp)
          IF exitlab DO out2(s_lab, exitlab)
          IF next>0 DO out2(s_jump, next)
          IF next<0 DO out1(s_rtrn)
          ENDCASE

        CASE s_everye:
          ssp := patresultpos+1
	  out2(s_stack, ssp)
outcomment("EVERYe final code, patresultpos=%n*n", patresultpos)
          IF exitlab DO out2(s_lab, exitlab)

          IF next>0 DO out2(s_jump, next)
          IF next<0 DO out1(s_fnrn)
          ENDCASE
      }

      matchcontext  := prevmatchcontext
      nextmatchitem := prevnextmatchitem
      patresultpos  := prevpatresultpos
      exitlab       := prevexitlab
      nextlab       := prevnextlab
      casecount     := prevcasecount
      RETURN
    }
  }
}

AND declpatnames(plist, argpos, depth, path) BE IF plist DO
{ // plist is the pattern list
  // argpos, depth, path    give the address of the current location
  // This function searches for pattern variables to declare.
  LET op = h1!plist
  LET k = s_local

  // Check that depth and path are allowable
  IF depth>4 DO
    trnerr("Pattern depth is not allowed to be greater than 4")
  IF (path & 255) > 254 DO
    trnerr("The pattern list is too long")
    
  SWITCHON op INTO
  { DEFAULT:
      RETURN                // All other pattern operators

    CASE s_flt:
      plist := h2!plist     // x should point to [s_flt, [s_name, ... ]]
      op  := h1!plist
      UNLESS op=s_name DO
         trnerr("SYSTEM ERROR in declpatnames, FLT not followed by a name")
      k := s_flocal
      // Fall through to CASE s_name
      
    CASE s_name:
    { LET cell = cellwithname(plist)

      // Manifest names are not pattern variables
      IF (h2!cell & s_fltmask) = s_manifest RETURN
      
      // The name is not a manifest so define it as a path variable.
      
      IF depth=0 DO { // Declare a simple pattern variable
                      addname(plist, k, argpos, 0)
                      IF xrefing DO
                        xref(plist, (k=s_local -> "P:", "FP:"), argpos, k)
//writef("Declaring pattern variable %s to be %s %n*n",
//        @h3!plist, opname(k), argpos)
                      RETURN
                    }
      TEST k=s_local
      THEN k := s_path1  + depth - 1
      ELSE k := s_fpath1 + depth - 1

      // Declare a path pattern variable
      addname(plist, k, argpos, path)
      IF xrefing DO xref(plist, (k=s_local -> "I:", "FI:"), argpos, k, path)
//writef("Declaring pattern variable %s to be %s %n path=%x8*n",
//        @h3!plist, opname(k), argpos, path)
      RETURN
    }
    
    CASE s_comma:
       declpatnames(h2!plist, argpos, depth, path)
       TEST depth=0
       THEN declpatnames(h3!plist, argpos+1, depth, path)
       ELSE declpatnames(h3!plist, argpos,   depth, path+1)
       RETURN

    CASE s_patand:
    CASE s_pator:
       declpatnames(h2!plist, argpos, depth, path)
       declpatnames(h3!plist, argpos, depth, path)
       RETURN

    CASE s_patptr:
       declpatnames(h2!plist, argpos, depth+1, path<<8)
       RETURN

    CASE s_patseq:
       declpatnames(h2!plist, argpos, depth, path)
       RETURN
  }
}

AND transpattern(x, argpos, depth, path, B, n) BE
{ // When this is called matchcontext holds one of the following:
  // s_patfndef, s_patrtdef,
  // s_matche,   s_matchc,
  // s_everye or s_everyc

  // argpos, depth and path specify the position of the location/s
  // being tested.


  // B=TRUE   Compile code to jump to the destination specified
  //          by n if the pattern x is satisfied.
  // B=FALSE  Compile code to jump to the destination specified
  //          by n if the pattern x is not satisfied.

  // n specifies the destination of the jump as follows

  // If n>0 the destination is label n.
  // If n<0 Compile either LN 0; FNRN or a jump to retlab,
  //        allocating retlab if necessary.
  // If n=0 Compile code depending on matchcontext and
  //        other global variables.

  // x = 0 or                   // Equivalent to query
  //   -> [ comma,  P, P ]      // P , P
  //   |  [ pator,  P, P ]      // P | P
  //   |  [ patand, P, P ]      // P P
  //   |  [ patseq, P, P ]      // ( P )
  //   |  [ patptr, P, P ]      // [ P ]
  //   |  [ patrelop,  E ]      // relop E
  //   |  [ range,  E, E ]      // E .. E
  //   |  [ flt name ]          // FLT name
  //   |  name                  // A manifest or non manifest name
  //   |  [ query ]             // ?
  //   |  a constant (normally a possibly signed number)

  LET op = h1!x
  LET ff = FALSE       // Assume integer tests unless otherwise specified
  LET patrelop = s_eq
  
//sawritef("transpattern: op=%s argpos=%n depth=%n path=%X8 B=%n n=%n*n",
//          opname(op), argpos, depth, path, B, n)
//abort(1123)

  UNLESS x GOTO patsucc
   
  IF n=0 DO
  { IF nextlab=0 DO nextlab := genlab()
    n := nextlab
  }

  //IF n<0 TEST retlab~=-2
  //THEN { UNLESS retlab DO retlab := genlab()
  //       n := retlab
  //     }
  //ELSE { UNLESS ret0lab DO ret0lab := genlab()
  //       n := ret0lab
  //     }
  
  //// n is now a positive label.
  
//sawritef("transpattern: n now =%n nextlab=%n retlab=%n*n",
//          n, nextlab, retlab)
//abort(1124)
  
  SWITCHON op INTO
  { DEFAULT:
      trnerr("System error in transpattern, op=%n", op)
      RETURN

    CASE s_flt:       // FLT is only allowed in declarations
      IF isconst(h2!x) DO
        trnerr("*nThe name following FLT in a pattern must not be*n*
                * a manifest constant")
      // This name is being declared as a pattern variable
      // so always succeeds. Treat it as a query.

    CASE s_query:     // query always succeeds
patsucc:
      IF B DO
      { IF n>0 DO out2(s_jump, n)
        IF n<0 TEST proccontext=s_rtrn
	       THEN out1(s_rtrn)
	       ELSE out3(s_ln, 0, s_fnrn)
      }
      RETURN

    CASE s_name:
      UNLESS isconst(x) DO
      { // x is a non manifest name so is are pattern local
        // variable declarations and always succeeds.
        GOTO patsucc
      }
      // x is a manifest constant
      // Fall through

    CASE s_pos:
    CASE s_fpos:
    CASE s_neg:
    CASE s_fneg:
    CASE s_number:
    CASE s_fnum:
    CASE s_true:
    CASE s_false:
    //sawritef("transpattern: constants ssp=%n*n", ssp)
    { LET L = genlab()
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(x, FALSE)  // Load the manifest value
      out1(s_eq)
      ssp := ssp-1
      out2(B->s_jt,s_jf, n)
      ssp := ssp-1
    //sawritef("transpattern: constants ssp=%n after JT or JF*n", ssp)
      RETURN
    }

    CASE s_patfeq:
    CASE s_patfne:
    CASE s_patfls:
    CASE s_patfgr:
    CASE s_patfle:
    CASE s_patfge:
      ff := TRUE
    CASE s_pateq:
    CASE s_patne:
    CASE s_patls:
    CASE s_patgr:
    CASE s_patle:
    CASE s_patge:
      UNLESS ff IF isflt(h2!x) DO
      { op := cv2flt(op) // Promote to floating point if needed
        h1!x := op
        ff := TRUE
      }
      oppath(s_lp, argpos, depth, path)
      ssp := ssp+1
      load(h2!x, FALSE)
      out1(patrel2rel(op))
      ssp := ssp-1
      out2(B->s_jt,s_jf, n)
      ssp := ssp-1
      RETURN

    CASE s_comma:  // x -> [ comma, P, P ]
      TEST B
      THEN { transpattern(h2!x, argpos, depth, path, FALSE, n)
             TEST depth
             THEN transpattern(h3!x, argpos,   depth, path+1, TRUE, n)
             ELSE transpattern(h3!x, argpos+1, depth, path,   TRUE, n)
           }
      ELSE { transpattern(h2!x, argpos, depth, path, FALSE, n)
             TEST depth
             THEN transpattern(h3!x, argpos,   depth, path+1, FALSE, n)
             ELSE transpattern(h3!x, argpos+1, depth, path,   FALSE, n)
	   }
      RETURN

    CASE s_pator:  // x -> [ pator, P, P ]
      TEST B
      THEN { transpattern(h2!x, argpos, depth, path, TRUE,  n)
             transpattern(h3!x, argpos, depth, path, TRUE,  n)
           }
      ELSE { LET m = genlab()
             transpattern(h2!x, argpos, depth, path, TRUE,  m)
             transpattern(h3!x, argpos, depth, path, FALSE, n)
	     out2(s_lab, m)
	   }
      RETURN

    CASE s_patand:  // x -> [ patand, P, P ]
      TEST B
      THEN { LET m = genlab()
             transpattern(h2!x, argpos, depth, path, FALSE, m)
             transpattern(h3!x, argpos, depth, path, TRUE,  n)
	     out2(s_lab, m)
           }
      ELSE { transpattern(h2!x, argpos, depth, path, FALSE, n)
             transpattern(h3!x, argpos, depth, path, FALSE, n)
	   }
      RETURN

    CASE s_patptr:  // x -> [ patptr, P ]
      transpattern(h2!x, argpos, depth+1, path<<8, B, n)
      RETURN

    CASE s_patseq:  // x -> [ patseq, P ]
      transpattern(h2!x, argpos, depth, path, B, n)
      RETURN

    CASE s_break:
    CASE s_loop:
    CASE s_endcase:
    CASE s_next:
    CASE s_exit:
      trans(x, 0)
      RETURN
      
    CASE s_frange: // x -> [ #.., E, E ]
      ff := TRUE
    CASE s_range:  // x -> [ #.., E, E ]
      UNLESS ff IF isflt(h2!x) | isflt(h3!x) DO
      { op := cv2flt(op) // Promote to floating point if needed
        h1!x := op
	ff := TRUE
      }
      // P  .. Q  is equivalent to   >=P  <=Q
      // P #.. Q  is equivalent to  #>=P #<=Q

      TEST B
      THEN { // B is TRUE
	     // If current value <  P jump to Lm
	     // If current value <= Q jump to Ln
	     // set label M

             LET m = genlab()
             oppath(s_lp, argpos, depth, path)
             // A = the current value
             ssp := ssp+1
             load(h2!x, ff)
             out1(ff -> s_fls, s_ls)
             ssp := ssp-1
             out2(s_jt, m)
             ssp := ssp-1
             oppath(s_lp, argpos, depth, path)
             ssp := ssp+1
             load(h3!x, ff)
             out1(ff -> s_fle, s_le)
             ssp := ssp-1
             out2(s_jt, n)
             ssp := ssp-1
             out2(s_lab, m)
             RETURN
	   }
      ELSE { // B is FALSE
	     // If current value < P jump to n
	     // If current value > Q jump to n

             oppath(s_lp, argpos, depth, path)
             // A = the current value
             ssp := ssp+1
             load(h2!x, ff)
             out1(ff -> s_fls, s_ls)
             ssp := ssp-1
             out2(s_jt, n)
             ssp := ssp-1
             oppath(s_lp, argpos, depth, path)
             ssp := ssp+1
             load(h3!x, ff)
             out1(ff -> s_fgr, s_gr)
             ssp := ssp-1
             out2(s_jt, n)
             ssp := ssp-1
             RETURN
	   }
  }
}

AND oppath(op,        // s_lp, s_llp or s_sp
           pos,
           depth,
	   path) BE
{ // Load or store a value specified by a path onto/from the top of the stack.
  // This function does not change ssp. That is done in the calling code.
//writef("oppath: pos=%n depth=%n path=%X8*n", pos, depth, path)
//abort(5524)
  LET offset = path&255
  
  IF depth<=0 DO
  { out2(op, pos)
    RETURN
  }
  oppath(s_lp, pos, depth-1, path>>8)
  IF offset DO
  { out2(s_ln, offset)
    out1(s_add)
  }
  IF op=s_lp DO
  { out1(s_rv)
    RETURN
  }
  IF op=s_sp DO
  { out1(s_stind)
    RETURN
  }
  IF op=s_llp RETURN
  trnerr("SYSTEM ERROR: in oppath")
}

AND statdefs(x) = h1!x=s_fndef | h1!x=s_rtdef       -> TRUE,
                  h1!x=s_patfndef | h1!x=s_patrtdef -> TRUE,
                  h1!x ~= s_and                     -> FALSE,
                  statdefs(h2!x)                    -> TRUE,
                  statdefs(h3!x)

AND choosereturnlab() = VALOF
{ // Exactly one of the following should be not equal -2
  //    retlab       returning from a routine
  //    ret0lab      returning from funcion with result 0 
  
  //IF retlab = 0 DO retlab := genlab()
  //IF retlab>0 RESULTIS retlab

  //IF ret0lab = 0 DO ret0lab := genlab()
  //IF ret0lab>0 RESULTIS ret0lab

  RESULTIS 0
}

LET jumpcond(x, b, L) BE
{ // L is an allocated label number > 0
  LET sw = b

  SWITCHON h1!x INTO
  { CASE s_false:  b := NOT b
    CASE s_true:   IF b DO out2(s_jump, L)
                   RETURN
 
    CASE s_not:    jumpcond(h2!x, NOT b, L)
                   RETURN
 
    CASE s_logand: sw := NOT sw
    CASE s_logor:  TEST sw THEN { jumpcond(h2!x, b, L)
                                  jumpcond(h3!x, b, L)
                                  RETURN
                                }
 
                           ELSE { LET M = genlab()
                                  jumpcond(h2!x, NOT b, M)
                                  jumpcond(h3!x, b, L)
                                  out2(s_lab, M)
                                  RETURN
                                }
 
    DEFAULT:       load(x, FALSE)
                   out2(b -> s_jt, s_jf, L)
                   ssp := ssp - 1
                   RETURN
  }
}
 
AND transswitch(x, next) BE
{ LET cl, cc = caselist, casecount // These must be saved and restored 
  LET dl, el = defaultlab, endcaselab
  LET next1 = 0       // This will hold next or a label after
                      // the SWITCHON statement
  LET L = genlab()    // Labelling the Ocode SWITCHON statement
  LET dlab = 0        // This will hold defaultlab or a label after
                      // the Ocode SWITCHON statement
  
  casecount, defaultlab := 0, 0 // DEFAULT and CASE labels
                                // are allowed.
  caselist := 0 // No CASE labels yet.
  
  // Note the possible values of next are:
  // =0  continue execution just after the SWITCHON command.
  // >0  code equivalent to JUMP L(next) after the SWITCHON command.
  // =-1 compile code to return from the current function
  //     or routine after the SWITCHON command based on the
  //     values of retlab and ret0lab.
  // =-2 system error

  // If next=0 the code must jump around the SWITCHON statement.
  // This is done placing a newly allocated label in next1 and
  // compiling the LAB statement for next1 after compiling
  // the Ocode SWITCHON statement.
  
  endcaselab := next // A value >0, =0 or =-1
  endcaselab := genlab() // A value >0
  next1 := next>0 -> next, genlab()
  // next1 is normally labels the first instruction after
  // the Ocode SWITCHON statement, but equals next if
  // next is greater than 0
  
  context, comline := x, h4!x

  load(h2!x, FALSE)  // Evaluate the switch expression

  out2(s_res, L) // Make a jump to the end of the switch
                 // body with the switch expression in <res>
		 // where it will be accessed by an RSTACK
		 // statement. This may avoid the switch 
		 // value being copied into a stack location.
  ssp := ssp-1

  // Compile the switch body collecting the case label data

  trans(h3!x, next1) // next1 is used to normally jump around
                     // the SWITCHON statement, but will jump
		     // to next if next is greaer than 0.

  // Since next1 is non zero this position in the code will
  // not be reached from the compilation of h3!x

  context, comline := x, h4!x
  
  // Choose the default label number for the SWITCHON statement
  IF defaultlab>0 DO dlab := defaultlab  // DEFAULT: was present
  IF defaultlab=0 &                      // DEFAULT: not present
     next>0       DO dlab := next        // and next is a specified label
	                              
  UNLESS dlab     DO dlab := genlab()    // Otherwise allocate a label

  out2(s_lab, L)      // The switch value is on the top of the stack
  out2(s_rstack, ssp) // Load <res> onto the top of the stack
  ssp := ssp+1

  // The switch expression value is on the top of the stack
  out3(s_switchon, casecount, dlab)
  //IF hard DO
  //{ sawritef("transsawitch: casecount=%n caselist=%n*n",
  //                    casecount, caselist)
  //  abort(3323)
  //}
  WHILE caselist DO { out2(h2!caselist, h3!caselist)
                      caselist := h1!caselist
                    }
  ssp := ssp-1

  { LET flag = FALSE // Will be TRUE if a label is set
                     // after the Ocode SWITCHON statement
  
    // next1 is >0 and if equal to next there is no need to
    // set the next1 label.
    UNLESS next1=next DO
    { out2(s_lab, next1)
      flag := TRUE
    }

    // Compile a LAB statement for the destination of ENDCASE,
    // if necessary.
    IF endcaselab>0 &
      endcaselab ~= next DO
      { out2(s_lab, endcaselab)
        flag := TRUE
      }

    // Compile a LAB statement for the DEFAULT label, if necessary.
    IF defaultlab=0 & dlab ~= next DO
      { out2(s_lab,  dlab)
        flag := TRUE
      }

    IF flag DO trnext(next)
  }

  defaultlab, endcaselab := dl, el
  caselist,   casecount  := cl, cc
}
 
AND transfor(x, next) BE
{ // x -> [s_for, N, initval, lim, step, c, ln]
  LET e, m, blab = dvece, genlab(), genlab()
  LET bl, ll = breaklab, looplab
  // Note: ENDCASE is allowed in FOR commands
  LET cc = casecount
  LET k, n, step = 0, 0, 1
  LET s = ssp
  LET name = h2!x

  casecount := -1  // Disallow CASE and DEFAULT labels.   
  breaklab, looplab := genlab(), genlab()
   
  context, comline := x, h7!x
 
  IF h1!name=s_flt DO
  { trnerr("FOR loop control variable must not have the FLT tag")
    name := h2!name  // Remove the FLT tag
    h2!x := name
  }

  addname(name, s_local, s, 0)
  load(h3!x, FALSE)       // The initial value

  // Set k, n to be the instruction to load the end limit, if there is one
  // k is zero if no end limit was specified.???
  IF h4!x TEST h1!(h4!x)=s_number
          THEN   k, n := s_ln, h2!(h4!x)
          ELSE { k, n := s_lp, ssp
                 load(h4!x, FALSE) // Place the end limit in the stack
               }
  // k=0 if there is no TO expression
  
  IF h5!x DO step := evalconst(h5!x, FALSE) // Set step if BY given
 
  out1(s_store)  // Ensure the control variable and possible end limit
                 // is stored in memory
   
  TEST k=s_ln & h1!(h3!x)=s_number  // check for constant limit expression
  THEN { // The initial and limit values are both constants 
         LET initval = h2!(h3!x)
         IF step>=0 & initval>n | step<0 & initval<n DO
         { // The body of this FOR loop will not be executed
	   TEST next<0
           THEN out1(s_rtrn)
           ELSE TEST next>0
                THEN out2(s_jump, next)
                ELSE { //blab := breaklab>0 -> breaklab, genlab()
                       out2(s_jump, blab)
                     }
         }
       }
  ELSE { //IF next<=0 DO blab := genlab()
         // Only perform a conditional jump if the TO expression was given.
	 IF k DO
         { out2(s_lp, s)
           out2(k, n)
           out1(step>=0 -> s_gr, s_ls)
           out2(s_jt, next>0 -> next, blab)
	 }
       }

  //IF breaklab=0 & blab>0 DO breaklab := blab
   
  context, comline := x, h7!x
  out2(s_lab, m)
  decllabels(h6!x)
  trans(h6!x, 0)   // Translate the body of the for loop.

  IF looplab>0 DO out2(s_lab, looplab)

  // Compile code to increment the control variable
  out2(s_lp, s); out2(s_ln, step); out1(s_add); out2(s_sp, s)
  TEST k
  THEN { // If the TO expression is given compile the conditional jump.
         out2(s_lp,s); out2(k,n); out1(step>=0 -> s_le, s_ge)
         out2(s_jt, m)
       }
  ELSE { // No TO expression given so compile an unconditional jump
         out2(s_jump, m)
       }
 
  //IF next<=0 TEST blab>0 
  //           THEN                  out2(s_lab, blab)
  //           ELSE IF breaklab>0 DO out2(s_lab, breaklab)
  IF breaklab>0 DO out2(s_lab, breaklab)
  IF blab>0 DO out2(s_lab, blab)
  trnext(next)
  casecount := cc
  breaklab, looplab, ssp := bl, ll, s
  out2(s_stack, ssp)
  undeclare(e)
}

LET isflt(x) = x=0 -> FALSE, VALOF
{ // Return TRUE if expression x is an fnumber, a name declared
  // with the FLT tag or has a leading operator such as #+ or #-
  // that returns a floating point value. Remember the operators
  // such as + and - are converted to #+ and #- if they have
  // floating point operands.
  SWITCHON h1!x INTO
  { DEFAULT:  RESULTIS FALSE

    CASE s_name: { LET c = cellwithname(x)
                   IF (h2!c & s_fltbit)=0 RESULTIS FALSE
                   RESULTIS TRUE
                 }

    CASE s_float: CASE s_fabs:
    CASE s_fpos:  CASE s_fneg:
    CASE s_fadd:  CASE s_fsub:
    CASE s_fmul:  CASE s_fdiv: CASE s_fmod:
    CASE s_fcond:
    CASE s_fnum:  RESULTIS TRUE

    CASE s_pos:CASE s_neg: CASE s_abs:
      RESULTIS isflt(h2!x)

    CASE s_add: CASE s_sub:
    CASE s_mul: CASE s_div: CASE s_mod:
      IF isflt(h2!x) | isflt(h3!x) RESULTIS TRUE
      RESULTIS FALSE

    CASE s_cond:
      IF isflt(h3!x) | isflt(h4!x) RESULTIS TRUE
      RESULTIS FALSE
  }    
}
 
LET load(x, ff) BE
{ // Translate expression x into Ocode.
  // The compiled code will load one value on the runtime stack.
  // If ff=TRUE, the expression is in an FLT context and will
  // convert, for example, + and - to #+ and #-.
  LET op = h1!x

  IF isconst(x) DO
  { out2(s_ln, evalconst(x, ff | isflt(x)))
    ssp := ssp + 1
    RETURN
  }
 
  SWITCHON op INTO
  { DEFAULT:
           trnerr("Compiler error in Load, op=%s", opname(op))
           out2(s_ln, 0)
           ssp := ssp + 1
           RETURN

    CASE s_break:
    CASE s_loop:
    CASE s_endcase:
    CASE s_next:
    CASE s_exit:
    CASE s_return:
           trans(x, 0)
	   ssp := ssp+1       // Because every expression 'loads'
	   out2(s_stack, ssp) // one value on the stack.
	   RETURN
	   
    CASE s_of:
         { LET slct = evalconst(h2!x, FALSE) // Inserted 11/7/01
           LET len = slct>>24
           LET sh  = slct>>16 & 255
           LET offset = slct & #xFFFF
           load(h3!x, FALSE)
	   
           IF offset DO
           { out2(s_ln, offset)
             out1(s_add)
           }

           // Optimise accessing a complete word.
           IF sh=0 & (len=0 | len=wordbitlen) DO
           { out1(s_rv) // The source field is a complete word
             RETURN
           }

           // Compile (SLCT len:sh:0)(E+offset)
           TEST noselst
           THEN { // Old version not using SELLD
                  out1(s_rv)
                  IF sh DO
                  { out2(s_ln, sh)
                    out1(s_rshift)
                  }
                  IF len>0 & (len+sh~=wordbitlen) DO
                  { // Applying a mask is necessary
                    LET mask = (1<<len)-1
                    out2(s_ln, mask)
                    out1(s_logand)
                  }
                }
           ELSE { // New version using SELLD
                  out3(s_selld, len, sh)
                }
           RETURN
         }

    CASE s_div: CASE s_mod: CASE s_sub:
         // Convert to floating point if in FLT mode or
         // has a floating point operand.
         IF ff | isflt(x) DO
         { // Convert to floating point operators.
           h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         load(h2!x, FALSE)
         load(h3!x, FALSE)
         out1(op)
         ssp := ssp - 1
         RETURN
                      
    CASE s_fdiv: CASE s_fmod: CASE s_fsub:
         load(h2!x, TRUE)
         load(h3!x, TRUE)
         out1(op)
         ssp := ssp - 1
         RETURN

    CASE s_ls: CASE s_gr: CASE s_le: CASE s_ge:
         // Only convert to floating point if they have
         // a floating point operand.
         IF isflt(h2!x) | isflt(h3!x) DO
         { // Convert to floating point operators.
           h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         load(h2!x, FALSE)
         load(h3!x, FALSE)
         out1(op)
         ssp := ssp - 1
         RETURN

    CASE s_fls: CASE s_fgr: CASE s_fle: CASE s_fge:
         load(h2!x, TRUE)
         load(h3!x, TRUE)
         out1(op)
         ssp := ssp - 1
         RETURN
 
    CASE s_byteap:
         load(h2!x, FALSE)
         load(h3!x, FALSE)
         out1(s_getbyte)    // Compiling: E1%E2
         ssp := ssp - 1
         RETURN

    CASE s_lshift: CASE s_rshift:
         load(h2!x, FALSE)  // Compiling: E1<<E2  or  E1>>E2
         UNLESS iszero(h3!x,FALSE) DO
         { load(h3!x, FALSE)
           out1(op)
           ssp := ssp - 1
         }
         RETURN
 
    CASE s_eq: CASE s_ne:
         // Relational operators are only converted if they
         // have floating point operands.
         IF isflt(h2!x) | isflt(h3!x) DO
         { // Convert to floating point.
           h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         GOTO intsymmetric

    CASE s_mul: CASE s_add:
          // Convert to floating point if in FLT mode or
          // has a floating point operand.
         IF ff | isflt(x) DO
         { h1!x := cv2flt(op)
           load(x, TRUE)
           RETURN
         }
         // Fall through

    CASE s_vecap:
    CASE s_logand: CASE s_logor: CASE s_eqv: CASE s_xor:
intsymmetric:
       // Symmetric non FLT dyadic operators.
       { LET a, b = h2!x, h3!x
         TEST h1!a=s_name   |
              h1!a=s_number |
              h1!a=s_fnum THEN { load(b, FALSE); load(a, FALSE) }
                          ELSE { load(a, FALSE); load(b, FALSE) }
         TEST op=s_vecap THEN out2(s_add, s_rv)
                         ELSE out1(op)
         ssp := ssp - 1
         RETURN
       }
 
    CASE s_fmul: CASE s_fadd: CASE s_feq: CASE s_fne:
       { LET a, b = h2!x, h3!x
         TEST h1!a=s_name   |
              h1!a=s_number |
              h1!a=s_fnum THEN { load(b, TRUE); load(a, TRUE) }
                          ELSE { load(a, TRUE); load(b, TRUE) }
         out1(op)
         ssp := ssp - 1
         RETURN
       }
 
     CASE s_pos: CASE s_neg: CASE s_abs:
       IF ff | isflt(x) DO
       { h1!x := cv2flt(op)
         load(x, TRUE)
         RETURN
      }
      load(h2!x, FALSE)
      UNLESS op=s_pos DO out1(op)
      RETURN

    CASE s_fpos:
      load(h2!x, TRUE)
      RETURN
 
    CASE s_fneg: CASE s_fabs:CASE s_fix:
      load(h2!x, TRUE)
      out1(op)
      RETURN
 
    CASE s_float: CASE s_not: CASE s_rv:
      load(h2!x, FALSE)
      out1(op)
      RETURN
 
    CASE s_true: CASE s_false: CASE s_query:
       out1(op)
       ssp := ssp + 1
       RETURN
 
    CASE s_lv:
       loadlv(h2!x)
       RETURN
 
    CASE s_number:
//sawritef("number %n  ff=%n*n", h2!x, ff)
       IF ff DO
       { // Convert the integer constant to floating point
         h1!x := s_fnum
         h2!x := sys(Sys_flt, fl_mk, h2!x, 0)
//sawritef("number converted to fnumber %13e*n", h2!x)
       }
       // Fall through
    CASE s_fnum:
       out2(s_ln, h2!x)
       ssp := ssp + 1
       RETURN
 
    CASE s_string:
       out1(s_lstr)
       outstring(@ h2!x)
       ssp := ssp + 1
       RETURN
 
    CASE s_name:
       transname(x, s_lp, s_lg, s_ll, s_lf, s_ln)
       ssp := ssp + 1
       RETURN
 
    CASE s_valof:
     { LET e, rl, cc = dvece, resultlab, casecount
       casecount := -2 // Disallow CASE & DEFAULT labels
       resultlab := genlab()
       decllabels(h2!x)
       trans(h2!x, 0) // Translate the VALOF body only
       out2(s_lab, resultlab)
       out2(s_rstack, ssp)
       ssp := ssp + 1
       resultlab, casecount := rl, cc
       undeclare(e)
       RETURN
     }
 
    CASE s_matche: // In load(x, ff)
    { // This will load one item, so EXIT will load zero and NEXT
      // in the last match item will do the same. If the pattern
      // in the last match item fails the result is also zero.
      // The expression of the first successful match item will
      // return its value. All these values are returned using RES
      // and RSTACK just as they are used in the implementation
      // of RESULTIS in VALOF blocks. When compiling a MATCH
      // expression exitlab is always allocated and will label the
      // RSTACK statement. The escape commands BREAK, LOOP and
      // ENDCASE leave the MATCH expression without passing a value.
      
      LET argpos = ssp   // Position relative to P of first argument
      context, comline := x, h5!x
      loadlist(h2!x)
      out1(s_store) // Ensure that the arguments are in memory

      transmatchlist(s_matche,
                     h3!x,   // mlist -> [matchiteme, plist, E, link, ln]
                     argpos, // Position of the first match arg
		     0)      // This means fall through if all match
		             // items fail.
      //ssp := argpos
      //out2(s_rstack, ssp)
      //ssp := ssp+1
      RETURN
    }
 
    CASE s_everye:
    { LET prevpatresultpos = patresultpos
      LET argpos = ssp+2  // Position relative to P of first argument.

      context, comline := x, h5!x

      out2(s_ln, 0)       // Initialise the result location
      patresultpos := ssp
      ssp := ssp+1

      loadlist(h2!x)
      out1(s_store) // Ensure that the arguments are in memory

      transmatchlist(s_everye,
                     h3!x,           // List of match items.
                     patresultpos+1, // Position of the first match arg
		     0)              // This means leave the everye result
		                     // at position patresltpos.

      ssp := patresultpos+1
      //out2(s_stack, ssp)
      //out2(s_lp, patresultpos) 
      //ssp := ssp+1
      
      patresultpos := prevpatresultpos
      RETURN
    }

   
    CASE s_fnap:
     { LET s = ssp
       ssp := ssp + savespacesize
       out2(s_stack, ssp)
       loadlist(h3!x) // Load arguments in non FLT mode
       load(h2!x, FALSE)
       out2(s_fnap, s)
       ssp := s + 1
       RETURN
     }

    CASE s_fcond:
       ff := TRUE
       GOTO cond

    CASE s_cond:
       IF ff | isflt(x) DO
       { h1!x := s_fcond
         load(x, TRUE)
         RETURN
       }
cond:
     { LET l, m = genlab(), genlab()
       LET s = ssp
       jumpcond(h2!x, FALSE, m)
       load(h3!x, ff)
       out2(s_res,l)
       ssp := s; out2(s_stack, ssp)
       out2(s_lab, m)
       load(h4!x, ff)
       out2(s_res,l)
       out2(s_lab, l)
       out2(s_rstack,s)
       RETURN
     }
 
    CASE s_table:
     { LET m = genlab()
       out2(s_datalab, m)
       x := h2!x
       WHILE h1!x=s_comma DO
       { out2(s_itemn, evalconst(h2!x, FALSE))
         x := h3!x
       }
       out2(s_itemn, evalconst(x, FALSE))
       out2(s_lll, m)
       ssp := ssp + 1
       RETURN
     }
  }
}

AND fnbody(x, ff) BE SWITCHON h1!x INTO
{ // If ff is TRUE  compile x in FLT mode
  // If ff is FALSE compile x in non FLT mode
  // Compile expression x followed by FNRN
  // This is only used to compile the bodies of functions,
  // pattern functions and RESULTIS when the value is being returned
  // as the result of a function.
  
  DEFAULT:
    load(x, ff)
    out1(s_fnrn)
    ssp := ssp - 1
    RETURN

  CASE s_matche:
  { LET argpos = ssp   // Position relative to P of first argument
    context, comline := x, h5!x
      
    loadlist(h2!x)
    out1(s_store) // Ensure that the arguments are in memory

    transmatchlist(h1!x,   // The match context: s_matche or s_everye
                   h3!x,   // mlist -> [matchiteme, plist, E, link, ln]
                   argpos, // Position of the first match arg
                   -1)     // This means return the value as the
                           // result of this function.
    RETURN
  }
 
  CASE s_everye:
  { LET argpos = ssp+1   // Position relative to P of first argument
    patresultpos := ssp  // allowing space for the accumulated result.
    ssp := ssp+1
    out2(s_stack, ssp)
    // Initialise the EVERY expression result location.
    out2(s_ln, 0)
    out2(s_sp, patresultpos)
      
    context, comline := x, h5!x
      
    loadlist(h2!x)
    out1(s_store) // Ensure that the arguments are in memory

    transmatchlist(s_everye, // The match context
                   h3!x,     // mlist -> [matchiteme, plist, E, link, ln]
                   argpos,   // Position of the first match arg
                   -1)       // This means return the EVERY epression result
                             // as the result of the current function.
    RETURN
  }
 

  CASE s_valof:
  { LET e, rl, cc = dvece, resultlab, casecount
    casecount := -1 // Disallow CASE & DEFAULT labels
    resultlab := -1 // RES replaced by FNRN
    decllabels(h2!x)
    trans(h2!x, -1) // Compile the command as the body
                    // of a function. RESULTIS commmands
                    // return the function result
                    // other commands return
                    // undefined results.
    resultlab, casecount := rl, cc
    undeclare(e)
    RETURN
  }

  CASE s_fcond:
  { LET l = genlab()
    jumpcond(h2!x, FALSE, l)
    fnbody(h3!x, TRUE)
    out2(s_lab, l)
    fnbody(h4!x, TRUE)
    RETURN
  }

  CASE s_cond:
  { LET l = genlab()
    IF ff | isflt(x) DO
    { h1!x := s_fcond    // Promote -> to #->
      fnbody(x, TRUE)
      RETURN
    }
    jumpcond(h2!x, FALSE, l)
    fnbody(h3!x, ff)
    out2(s_lab, l)
    fnbody(h4!x, ff)
    RETURN
  }
}
 
 
AND loadlv(x) BE
{ UNLESS x=0 SWITCHON h1!x INTO
  { DEFAULT:         ENDCASE
 
    CASE s_name:     transname(x, s_llp, s_llg, s_lll, 0, 0)
                     ssp := ssp + 1
                     RETURN
 
    CASE s_rv:       load(h2!x, FALSE)
                     RETURN
 
    CASE s_vecap: { LET a, b = h2!x, h3!x
                    TEST h1!a=s_name   |
                         h1!a=s_number |
                         h1!a=s_fnum THEN { load(b, FALSE); load(a, FALSE) }
                                     ELSE { load(a, FALSE); load(b, FALSE) }
                    out1(s_add)
                    ssp := ssp - 1
                    RETURN
                  }
  }

  trnerr("Ltype expression needed")
  out2(s_ln, 0)
  ssp := ssp + 1
}
 
AND loadlist(x) BE
{ // Load function, routine or MATCH arguments
//sawritef("loadlist: leading op=%s*n", opname(h1!x))
  UNLESS x=0 TEST h1!x=s_comma
             THEN { loadlist(h2!x)
	            loadlist(h3!x)
		  }
             ELSE load(x, FALSE)
}

// The conversion function are:
//    op2sfop      convert an expression op to a selst sfop
//    assop2op     convert op:= to op
//    cv2flt       convert an integer op or assignment op
//                 to the floating point version.
//    patrel2rel convert a pattern relation to an ordinary relation

AND op2sfop(op) = VALOF SWITCHON op INTO
{ DEFAULT:       sawritef("SYSTEM ERROR in op2sfop invalid op=%s*n",
                          opname(op))
		 abort(999)
                 RESULTIS op

  CASE s_none:   RESULTIS sf_none

  CASE s_vecap:  RESULTIS sf_vecap

  CASE s_mul:    RESULTIS sf_mul
  CASE s_div:    RESULTIS sf_div
  CASE s_mod:    RESULTIS sf_mod
  CASE s_add:    RESULTIS sf_add
  CASE s_sub:    RESULTIS sf_sub

  CASE s_fmul:   RESULTIS sf_fmul
  CASE s_fdiv:   RESULTIS sf_fdiv
  CASE s_fmod:   RESULTIS sf_fmod
  CASE s_fadd:   RESULTIS sf_fadd
  CASE s_fsub:   RESULTIS sf_fsub

  CASE s_lshift: RESULTIS sf_lshift
  CASE s_rshift: RESULTIS sf_rshift
  CASE s_logand: RESULTIS sf_logand
  CASE s_logor:  RESULTIS sf_logor
  CASE s_eqv:    RESULTIS sf_eqv
  CASE s_xor:    RESULTIS sf_xor

}

AND assop2op(op) = VALOF SWITCHON op INTO
{ DEFAULT:       sawritef("Syserr in assop2op unknown op=%s*n",
                          opname(op))
                 RESULTIS op

//  CASE  0:       RESULTIS 0

  CASE s_assfmul:   RESULTIS s_fmul
  CASE s_assfdiv:   RESULTIS s_fdiv
  CASE s_assfmod:   RESULTIS s_fmod
  CASE s_assfadd:   RESULTIS s_fadd
  CASE s_assfsub:   RESULTIS s_fsub

  CASE s_assmul:    RESULTIS s_mul
  CASE s_assdiv:    RESULTIS s_div
  CASE s_assmod:    RESULTIS s_mod
  CASE s_assadd:    RESULTIS s_add
  CASE s_asssub:    RESULTIS s_sub

  CASE s_assvecap:  RESULTIS s_vecap
  CASE s_asslshift: RESULTIS s_lshift
  CASE s_assrshift: RESULTIS s_rshift
  CASE s_asslogand: RESULTIS s_logand
  CASE s_asslogor:  RESULTIS s_logor
  CASE s_asseqv:    RESULTIS s_eqv
  CASE s_assxor:    RESULTIS s_xor

  CASE s_fass:
  CASE s_ass:       RESULTIS s_none
}

/*
AND rel2patrel(op) = VALOF SWITCHON op INTO
{ DEFAULT:    writef("SYSTEM ERROR: in rel2patrel op=%s*n",
                     opname(op))
	      abort(999)
	      RESULTIS s_pateq
	      
  CASE s_eq:  RESULTIS s_pateq
  CASE s_feq: RESULTIS s_patfeq
  CASE s_ne:  RESULTIS s_patne
  CASE s_fne: RESULTIS s_patfne
  CASE s_le:  RESULTIS s_patle
  CASE s_fle: RESULTIS s_patfle
  CASE s_ge:  RESULTIS s_patge
  CASE s_fge: RESULTIS s_patfge
  CASE s_ls:  RESULTIS s_patls
  CASE s_fls: RESULTIS s_patfls
  CASE s_gr:  RESULTIS s_patgr
  CASE s_fgr: RESULTIS s_patfgr
}
*/
AND patrel2rel(op) = VALOF SWITCHON op INTO
{ DEFAULT:    writef("SYSTEM ERROR: in patrel2rel op=%s*n",
                     opname(op))
	      abort(999)
	      RESULTIS s_eq
	      
  CASE s_pateq:  RESULTIS s_eq
  CASE s_patfeq: RESULTIS s_feq
  CASE s_patne:  RESULTIS s_ne
  CASE s_patfne: RESULTIS s_fne
  CASE s_patle:  RESULTIS s_le
  CASE s_patfle: RESULTIS s_fle
  CASE s_patge:  RESULTIS s_ge
  CASE s_patfge: RESULTIS s_fge
  CASE s_patls:  RESULTIS s_ls
  CASE s_patfls: RESULTIS s_fls
  CASE s_patgr:  RESULTIS s_gr
  CASE s_patfgr: RESULTIS s_fgr
}

AND cv2flt(op) = VALOF SWITCHON op INTO
{ DEFAULT:       sawritef("Syserr in cv2flt op=%s not in switch*n",
                          opname(op))
                 RESULTIS op

  // Expression operators
  CASE s_pos:    RESULTIS s_fpos
  CASE s_neg:    RESULTIS s_fneg
  CASE s_abs:    RESULTIS s_fabs
  CASE s_number: RESULTIS s_fnum
  CASE s_mul:    RESULTIS s_fmul
  CASE s_div:    RESULTIS s_fdiv
  CASE s_mod:    RESULTIS s_fmod
  CASE s_add:    RESULTIS s_fadd
  CASE s_sub:    RESULTIS s_fsub
  CASE s_eq:     RESULTIS s_feq
  CASE s_ne:     RESULTIS s_fne
  CASE s_ls:     RESULTIS s_fls
  CASE s_gr:     RESULTIS s_fgr
  CASE s_le:     RESULTIS s_fle
  CASE s_ge:     RESULTIS s_fge
  CASE s_cond:   RESULTIS s_fcond

  // Pattern operators
  CASE s_range:  RESULTIS s_frange
  CASE s_pateq:  RESULTIS s_patfeq
  CASE s_patne:  RESULTIS s_patfne
  CASE s_patls:  RESULTIS s_patfls
  CASE s_patgr:  RESULTIS s_patfgr
  CASE s_patle:  RESULTIS s_patfle
  CASE s_patge:  RESULTIS s_patfge
  
  // Assignment operators
  CASE s_assmul: RESULTIS s_assfmul
  CASE s_assdiv: RESULTIS s_assfdiv
  CASE s_assmod: RESULTIS s_assfmod
  CASE s_assadd: RESULTIS s_assfadd
  CASE s_asssub: RESULTIS s_assfsub
  CASE s_ass:    RESULTIS s_fass
}

LET isconst(x) = VALOF
{ // Return TRUE if the expression x has a value that can
  // be determined at compile time. These are manifest names,
  // integer or floating point constants, the SLCT construct,
  // TRUE or FALSE, and any expression whose operands are constants
  // other than rv, vecap or byteap expressions.

  IF x=0 RESULTIS FALSE
 
  SWITCHON h1!x INTO
  { CASE s_name:
        { LET c = cellwithname(x)
          LET k = h2!c & s_fltmask
          RESULTIS k=s_manifest -> TRUE, FALSE
        }

    CASE s_fnum:
    CASE s_number:
    CASE s_slct:
    CASE s_true:
    CASE s_false:  RESULTIS TRUE
 
    CASE s_fneg:
    CASE s_fabs:
    CASE s_float:
    CASE s_fix:
    CASE s_pos:
    CASE s_neg:
    CASE s_abs:
    CASE s_not:    RESULTIS isconst(h2!x)
       
    CASE s_fmul:
    CASE s_fdiv:
    CASE s_fmod:
    CASE s_fadd:
    CASE s_fsub:
    CASE s_feq:
    CASE s_fne:
    CASE s_fls:
    CASE s_fgr:
    CASE s_fle:
    CASE s_fge:

    CASE s_mul:
    CASE s_div:
    CASE s_mod:
    CASE s_add:
    CASE s_sub:
    CASE s_lshift:
    CASE s_rshift:
    CASE s_logor:
    CASE s_logand:
    CASE s_eqv:
    CASE s_xor:
    CASE s_eq:
    CASE s_ne:
    CASE s_ls:
    CASE s_gr:
    CASE s_le:
    CASE s_ge:
                   UNLESS isconst(h2!x) RESULTIS FALSE
                   RESULTIS isconst(h3!x)

    CASE s_fcond:
    CASE s_cond:   UNLESS isconst(h2!x) RESULTIS FALSE
                   UNLESS isconst(h3!x) RESULTIS FALSE
                   RESULTIS isconst(h4!x)

    DEFAULT:       RESULTIS FALSE

  }
}

LET iszero(x, ff) = isconst(x) & evalconst(x, ff)=0 -> TRUE, FALSE

LET evalconst(x, ff) = VALOF
{ // If ff=TRUE the expression x is to be evaluated in
  // an FLT context, causing integer expression operators
  // to be automatically converted to their floating
  // point versions. Integer constants are also converted
  // to floating point.

  LET op, a, b = 0, 0, 0

  IF x=0 DO { trnerr("Compiler error in Evalconst")
              RESULTIS 0
            }

  IF isflt(x) DO ff := TRUE
 
  op := h1!x
//sawritef("evalconst: op=%s ff=%n*n", opname(op), ff)

  SWITCHON op INTO
  { CASE s_name: { LET c = cellwithname(x)
                   LET k = h2!c
                   LET a = h3!c
                   IF (k & s_fltmask)=s_manifest DO
                   { IF xrefing DO
                       xref(x,
                            (k=s_manifest -> "M:", "FM:"),
                            a, s_const)
                     RESULTIS a
                   }
                   TEST k
                   THEN trnerr("%s must be a MANIFEST constant", @h3!x)
                   ELSE trnerr("Name '%s' is not declared", @h3!x)
                   RESULTIS 0
                 }
 
    CASE s_number: UNLESS ff RESULTIS h2!x
                   // Convert from integer to floating point.
                   h1!x := s_fnum
                   h2!x := sys(Sys_flt, fl_mk, h2!x, 0)

    CASE s_fnum:   RESULTIS h2!x

    CASE s_true:   RESULTIS TRUE
    CASE s_false:  RESULTIS FALSE
    CASE s_query:  RESULTIS 0
 
    CASE s_slct: { LET len, sh, offset = 0, 0, 0     // Inserted 11/7/01
                   IF h2!x DO len    := evalconst(h2!x, FALSE)
                   IF h3!x DO sh     := evalconst(h3!x, FALSE)
                   IF h4!x DO offset := evalconst(h4!x, FALSE)
                   UNLESS 0<=len<=255 & 0<=sh<=255 & 0<=offset<=#xFFFF DO
                       trnerr("A field too large in a SLCT expression")
                   RESULTIS len<<24 | sh<<16 | offset
                 }

    CASE s_fpos:
    CASE s_fneg:
    CASE s_fabs:
    CASE s_fix:  
                   UNLESS t64=ON64 DO
                     trnerr("Compiler and target word length must be the same*
                            *for floating point numbers")
                   a := evalconst(h2!x, TRUE)
                   ENDCASE

    CASE s_pos:
    CASE s_neg:
    CASE s_abs:    IF ff | isflt(x) DO 
                   { h1!x := cv2flt(op)
                     RESULTIS evalconst(x, TRUE)
                   }
                   a := evalconst(h2!x, FALSE)
                   ENDCASE

    CASE s_not:       
    CASE s_float:  a := evalconst(h2!x, FALSE)
                   ENDCASE

    CASE s_fmul:
    CASE s_fdiv:
    CASE s_fmod:
    CASE s_fadd:
    CASE s_fsub:
    CASE s_feq:
    CASE s_fne:
    CASE s_fls:
    CASE s_fgr:
    CASE s_fle:
    CASE s_fge:
                 UNLESS t64=ON64 DO
                   trnerr("Compiler and target word length must be the same*
                          *for floating point numbers")
                 a, b := evalconst(h2!x, TRUE), evalconst(h3!x, TRUE)
                 ENDCASE

    CASE s_mul:
    CASE s_div:
    CASE s_mod:
    CASE s_add:
    CASE s_sub:  IF ff | isflt(x) DO
                 { // Convert to floating point.
                   h1!x := cv2flt(op)
                   RESULTIS evalconst(x, TRUE)
                 }
                 a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                 ENDCASE

    CASE s_eq:
    CASE s_ne:
    CASE s_ls:
    CASE s_gr:
    CASE s_le:
    CASE s_ge:   // Only convert to floating point if there is
                 // a floating point operand.
                 IF isflt(h2!x) | isflt(h3!x) DO
                 { // Convert to floating point.
                   h1!x := cv2flt(op)
                   RESULTIS evalconst(x, TRUE)
                 }
                 a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                 ENDCASE


    CASE s_lshift:
    CASE s_rshift:
    CASE s_logor:
    CASE s_logand:
    CASE s_eqv:
    CASE s_xor:    a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                   ENDCASE

    CASE s_fcond:  a, b := evalconst(h2!x, TRUE), evalconst(h3!x, TRUE)
                   ENDCASE

    CASE s_cond:   IF ff | isflt(x) DO
                   { // Convert to floating point.
                     h1!x := s_fcond
                     RESULTIS evalconst(x, TRUE)
                   }
                   a, b := evalconst(h2!x, FALSE), evalconst(h3!x, FALSE)
                   ENDCASE

    DEFAULT:
  }
    
  SWITCHON h1!x INTO
  { CASE s_pos:    RESULTIS     a
    CASE s_neg:    RESULTIS  -  a
    CASE s_abs:    RESULTIS ABS a
    CASE s_not:    RESULTIS NOT a
       
    CASE s_fpos:   RESULTIS a
    CASE s_fneg:   RESULTIS sys(Sys_flt, fl_neg,   a)
    CASE s_fabs:   RESULTIS sys(Sys_flt, fl_abs,   a)
    CASE s_fix:    RESULTIS sys(Sys_flt, fl_fix,   a)
    CASE s_float:  RESULTIS sys(Sys_flt, fl_float, a)
       
    CASE s_fmul:   RESULTIS sys(Sys_flt, fl_mul, a,  b)
    CASE s_fdiv:   RESULTIS sys(Sys_flt, fl_div, a,  b)
    CASE s_fmod:   RESULTIS sys(Sys_flt, fl_mod, a,  b)
    CASE s_fadd:   RESULTIS sys(Sys_flt, fl_add, a,  b)
    CASE s_fsub:   RESULTIS sys(Sys_flt, fl_sub, a,  b)

    CASE s_feq:    RESULTIS sys(Sys_flt, fl_eq, a,  b)
    CASE s_fne:    RESULTIS sys(Sys_flt, fl_ne, a,  b)
    CASE s_fls:    RESULTIS sys(Sys_flt, fl_ls, a,  b)
    CASE s_fgr:    RESULTIS sys(Sys_flt, fl_gr, a,  b)
    CASE s_fle:    RESULTIS sys(Sys_flt, fl_le, a,  b)
    CASE s_fge:    RESULTIS sys(Sys_flt, fl_ge, a,  b)

    CASE s_mul:    RESULTIS a   *   b
    CASE s_add:    RESULTIS a   +   b
    CASE s_sub:    RESULTIS a   -   b
    CASE s_lshift: RESULTIS a   <<  b
    CASE s_rshift: RESULTIS a   >>  b
    CASE s_logor:  RESULTIS a   |   b
    CASE s_logand: RESULTIS a   &   b
    CASE s_eqv:    RESULTIS a  EQV  b
    CASE s_xor:    RESULTIS a  XOR  b
    CASE s_div:    RESULTIS b=0 -> 0, a  /  b
    CASE s_mod:    RESULTIS b=0 -> 0, a MOD b
    CASE s_eq:     RESULTIS a =  b
    CASE s_ne:     RESULTIS a ~= b
    CASE s_ls:     RESULTIS a <  b
    CASE s_gr:     RESULTIS a >  b
    CASE s_le:     RESULTIS a <= b
    CASE s_ge:     RESULTIS a >= b

    CASE s_cond:   RESULTIS a -> b, evalconst(h4!x, FALSE)
    CASE s_fcond:  RESULTIS a -> b, evalconst(h4!x, TRUE)
   
    DEFAULT:       ENDCASE
  }

  trnerr("Error in manifest expression, op = %s", opname(h1!x))
  RESULTIS 0
}

AND assign(lhs, rhs, ff, op) BE
// Compile a simple assignment: lhs := rhs, lhs #:= rhs or lhs op := rhs.
// Note that for simultaneous assignments have already been replaced by
// sequences of simple assignments by cvassign.

// If op=s_none the assigment is either lhs := rhs or lhs #:= rhs,
// otherwise it is of the form: lhs op:= rhs where op is one of
// the dyadic expression operators allowed in assignments, namely:
//    s_vecap,
//    s_mul,   s_div,  s_mod,  s_add,  s_sub,
//    s_fmul,  s_fdiv, s_fmod, s_fadd, s_fsub,
//    s_lshift, s_rshift, s_logand, s_logor, s_eqv or s_xor.

// ff=TRUE if the rhs is to be evaluated in FLT mode.

{ LET sfop = op2sfop(op)
  // sfop is either sf_none or
  // an operator used in SELST Ocode instructions, namely:
  //   sf_vecap,
  //   sf_mul,  sf_div,  sf_mod,  sf_add,    sf_sub,
  //   sf_fmul, sf_fdiv, sf_fmod, sf_fadd or sf_fsub.

  SWITCHON h1!lhs INTO
  { CASE s_name:        // name op:= E
      TEST op=s_none
      THEN { // Compile: name := E
             load(rhs, ff)
             transname(lhs, s_sp, s_sg, s_sl, 0, 0)
             ssp := ssp - 1
           }
      ELSE { // Compile: name sfop:= E
             TEST noselst
             THEN { // Load: lhs op rhs
                    // op is a dyadic operator
                    LET operator, a, b = op, lhs, rhs
                    load(@operator, ff)
                    transname(lhs, s_sp, s_sg, s_sl, 0, 0)
                    ssp := ssp - 1
                  }
             ELSE { load(rhs, ff)
                    loadlv(lhs)
                    out4(s_selst, sfop, 0, 0)
                    ssp := ssp - 2
                  }
           }
      RETURN
 
    CASE s_rv:
    CASE s_vecap:  IF op=s_none DO
                   { load(rhs, ff)
                     loadlv(lhs)
                     out1(s_stind)
                     ssp := ssp - 2
                     RETURN
                   }

                   // op is a dyadic expression operator allowed
                   // in assignments.
                   IF noselst DO
                   { // Load: lhs op rhs
                     // op is a dyadic operator
                     LET operator, a, b = op, lhs, rhs
                     // Load the expression whose tree node is [op,lhs,rhs]
                     load(@operator, ff)
                     loadlv(lhs)
                     out1(s_stind)
                     ssp := ssp - 2
                     RETURN
                   }

                   // Compile using SELST
                   load(rhs, ff)
                   loadlv(lhs)
                   out4(s_selst, sfop, 0, 0) 
                   ssp := ssp - 2
                   RETURN

    CASE s_of:   { LET slct = evalconst(h2!lhs, FALSE) // Inserted 11/7/01
                   LET len = slct>>24
                   LET sh  = slct>>16 & 255
                   LET offset = slct & #xFFFF
                   LET mask = -1
                   IF len>0 DO mask := (1<<len)-1
                   mask := mask<<sh
                   TEST noselst
                   THEN { TEST op=s_none
                          THEN { load(rhs, ff)
                               }
                          ELSE { // Load: lhs op rhs
                                 // op is a dyadic operator
                                 LET operator, a, b = op, lhs, rhs
                                 load(@operator, ff)
                               }
                          IF sh DO
                          { out2(s_ln, sh)
                            out1(s_lshift)
                          }

                          UNLESS mask=-1 DO
                          { load(h3!lhs, FALSE)
                            IF offset DO
                            { out2(s_ln, offset)
                              out1(s_add)
                            }
                            out1(s_rv)
                            out1(s_xor)
                            ssp := ssp-1
                            out2(s_ln, mask)
                            out1(s_logand) // bits to change in x
                            load(h3!lhs, FALSE)
                            IF offset DO
                            { out2(s_ln, offset)
                              out1(s_add)
                            }
                            out1(s_rv)
                            out1(s_xor)
                            ssp := ssp-1
                          }

                          load(h3!lhs, FALSE)
                          IF offset DO
                          { out2(s_ln, offset)
                            out1(s_add)
                          }
                          out1(s_stind)
                        }
                   ELSE { // Compile using SELST
                          load(rhs, ff)
                          load(h3!lhs, FALSE)
                          IF offset DO
                          { out2(s_ln, offset)
                            out1(s_add)
                          }
                          TEST len=0 & sh=0 & sfop=sf_none
                          THEN out1(s_stind) // Full word field
                                             // and no op.
                          ELSE out4(s_selst, sfop, len, sh) 
                        }
                   ssp := ssp-2
                   RETURN
                 }

    CASE s_byteap:
      TEST op=s_none
      THEN { // Compiling: E1%E2 := E3
             load(rhs, ff)
           }
      ELSE { // Compiling: E1%E2 op:= E3
             // op is a dyadic expression operator allowed
             // in assignments.
             LET operator, a, b = op, lhs, rhs

             // The destination is not a full word field,
             // so some operators are not permitted.
             IF sfop=sf_fmul | sfop=sf_fdiv | sfop=sf_fmod |
                sfop=sf_fadd | sfop=sf_fsub | sfop=sf_vecap DO
               trnerr("Bad op in E%E op:= E")

             // Load an expression whose tree node is [op,lhs,rhs]
             load(@operator, FALSE)
           }
      load(h2!lhs, FALSE)
      load(h3!lhs, FALSE)
      out1(s_putbyte)
      ssp:=ssp-3
      RETURN

    DEFAULT:
      trnerr("Ltype expression needed")
  }
}
 
 
AND transname(x, p, g, l, f, n) BE
{ // x is a name node
  // p is s_lp, s_llp or s_sp   It determines the code for path variables
  LET c = cellwithname(x)
  LET k, a, path = h2!c, h3!c, h4!c
 
  // Must deal with s_fglobal, s_flocal, s_f_static, s_flabel, s_fmanifest
  // as if they were the integer versions. The sole purpose of these
  // is to indicate than a name has been declared with the FLT tag.
  // Note that s_fglobal = s_global  + s_fltbit,
  // and       s_global  = s_fglobal & s_fltmask, etc
  // where s_fltbit is 128 s_fltmask is 127.

  SWITCHON k INTO
  { DEFAULT:        trnerr("Name '%s' not declared", @h3!x)
   
    CASE s_fglobal:
    CASE s_global:  out2(g, a)
                    IF xrefing DO
                      xref(x,
                           ((k & s_fltbit)=0 -> "G:", "FG:"),
                           a, g)
                    RETURN
 
    CASE s_flocal:
    CASE s_local:   IF c<dvecp DO
                         trnerr("Dynamic free variable '%s' used", @h3!x)
                    out2(p, a)
                    //IF xrefing DO
                    //  xref(x,
                    //       ((k & s_fltbit)=0 -> "P:", "FP:"),
                    //       a, p)
                    RETURN
 
    CASE s_path1: CASE s_fpath1:
    CASE s_path2: CASE s_fpath2:
    CASE s_path3: CASE s_fpath3:
    CASE s_path4: CASE s_fpath4:
//   writef("transname: %s op=%s k=%s n=%n path=%x8*n",
//           @h3!x, opname(p), opname(k), n, path)
//   abort(5522)
                    IF c<dvecp DO
                         trnerr("Dynamic free variable '%s' used", @h3!x)
                    oppath(p,                      // s_lp, s_llp or s_sp
		           a,                      // argpos
		           (k&s_fltmask)-s_path1+1,// depth
		           path)                   // path
//   abort(5523)
                    //IF xrefing DO
                    //  xref(x,
                    //       ((k & s_fltbit)=0 -> "T:", "FT:"),
                    //       a, p)
                    RETURN
 
    CASE s_fstatic:
    CASE s_static:  out2(l, a)
                    IF xrefing DO
                      xref(x,
                           ((k & s_fltbit)=0 -> "S:", "FS:"),
                           a, l)
                    RETURN
 
    CASE s_label:   IF f=0 DO
                    { trnerr("Misuse of entry name '%s'", @h3!x)
                      f := p
                    }
                    out2(f, a)
                    IF xrefing DO xref(x, "F:", a, f)
                    RETURN

    CASE s_fmanifest:
    CASE s_manifest:IF n=0 DO
                    { trnerr("Misuse of MANIFEST name '%s'", @h3!x)
                      n := p
                    }
                    out2(n, a)
                    IF xrefing DO
                      xref(x,
                           ((k & s_fltbit)=0 -> "M:", "FM:"),
                           a, n)
  }
}

AND xref(x, kstr, n, op) BE
{ // Output a line of cross reference info
  // x is the name node
  // kstr, n describe how the name is being used
  // op decribes the context
  LET name = @h3!x
  LET fno = comline>>20
  LET lno = comline & #xFFFFF
  LET file = sourcenamev!fno
  writef("%s %s", name, kstr)
  TEST -10_000_000 <= n <= 10_000_000
  THEN writef("%n ", n)
  ELSE writef("#x%8x ", n)

  SWITCHON op INTO
  { DEFAULT:         writef("op%n", op); ENDCASE

    CASE s_fndef:    writef("FN");       ENDCASE
    CASE s_rtdef:    writef("RT");       ENDCASE
    CASE s_local:    writef("LOC");      ENDCASE
    CASE s_valdef:   writef("VAL");      ENDCASE
    CASE s_vecdef:   writef("VEC");      ENDCASE
    CASE s_constdef: writef("DEF");      ENDCASE
    CASE s_const:    writef("MAN");      ENDCASE
    CASE s_colon:    writef("LAB");      ENDCASE
    CASE s_sp:       writef("SP");       ENDCASE
    CASE s_sg:       writef("SG");       ENDCASE
    CASE s_sl:       writef("SL");       ENDCASE
    CASE s_llp:      writef("LLP");      ENDCASE
    CASE s_llg:      writef("LLG");      ENDCASE
    CASE s_lll:      writef("LLL");      ENDCASE
    CASE s_lp:       writef("LP");       ENDCASE
    CASE s_lg:       writef("LG");       ENDCASE
    CASE s_ll:       writef("LL");       ENDCASE
    CASE s_lf:       writef("LF");       ENDCASE
    CASE s_ln:       writef("LN");       ENDCASE
  }
  wrch(' ')
  IF file DO writef("%s", file)
  writef("[%n] ", lno)

  prctxt(context)

  newline()
}

AND prctxt(x) BE IF x DO 
{ LET op, str = h1!x, ""

  SWITCHON op INTO
  { DEFAULT:  prctxte(x, 7, 0); RETURN

    CASE s_fndef:
         writef("LET ")
         prctxte(h2!x, 7, 0)
         wrch('(')
         prctxte(h3!x, 7, 0)
         writef(")=..")
         RETURN

    CASE s_rtdef:
         writef("LET ")
         prctxte(h2!x, 7, 0)
         wrch('(')
         prctxte(h3!x, 7, 0)
         writef(")BE..")
         RETURN

    CASE s_valdef:
         writef("LET ")
         prctxte(h2!x, 6, 0)
         writef("=")
         prctxte(h3!x, 6, 0)
         RETURN

    CASE s_vecdef:
         writef("LET ")
         prctxte(h2!x, 6, 0)
         writef("=VEC ")
         prctxte(h3!x, 6, 0)
         RETURN

    CASE s_constdef:
         prctxte(h3!x, 6, 0)
         writef("=")
         prctxte(h4!x, 6, 0)
         RETURN

    CASE s_let:
         writef("LET ")
         prctxtd(h2!x, 6)
         writef("; ")
         prctxtc(h3!x, 6)
         RETURN
 
    CASE s_static:    writef("STATIC..");    RETURN
    CASE s_global:    writef("GLOBAL..");    RETURN
    CASE s_manifest:  writef("MANIFEST..");  RETURN


    CASE s_assvecap:  str := "!";    GOTO case_ass
    CASE s_assfmul:   str := "#**";  GOTO case_ass
    CASE s_assfdiv:   str := "#/";   GOTO case_ass
    CASE s_assfmod:   str := "#MOD"; GOTO case_ass
    CASE s_assfadd:   str := "#+";   GOTO case_ass
    CASE s_assfsub:   str := "#-";   GOTO case_ass
    CASE s_assmul:    str := "**";   GOTO case_ass
    CASE s_assdiv:    str := "/";    GOTO case_ass
    CASE s_assmod:    str := "MOD";  GOTO case_ass
    CASE s_assadd:    str := "+";    GOTO case_ass
    CASE s_asssub:    str := "-";    GOTO case_ass
    CASE s_asslshift: str := "<<";   GOTO case_ass
    CASE s_assrshift: str := ">>";   GOTO case_ass
    CASE s_asslogand: str := "&";    GOTO case_ass
    CASE s_asslogor:  str := "|";    GOTO case_ass
    CASE s_asseqv:    str := "EQV";  GOTO case_ass
    CASE s_assxor:    str := "XOR";  GOTO case_ass

    CASE s_fass:      str := "#";    GOTO case_ass

    CASE s_ass:       str := ""
case_ass:
         prctxte(h2!x, 4, 0)
         writef("%s:=", str)
         prctxte(h3!x, 4, 0)
         RETURN
 
    CASE s_rtap:
         prctxte(h2!x, 6, 12)
         writef("(")
         prctxte(h3!x, 6, 0)
         writef(")")
         RETURN
 
    CASE s_goto:
         writef("GOTO ")
         prctxte(h2!x, 6, 0)
         RETURN
 
    CASE s_colon:
         prctxte(h2!x, 6, 0)
         writef(":")
         prctxt(h3!x, 6)
         RETURN
 
    CASE s_unless:
    CASE s_if:
    CASE s_while:
    CASE s_until:
         writef(op=s_unless->"UNLESS ",
                op=s_if->"IF ",
                op=s_until->"UNTIL ",
                "WHILE "
               )
         prctxte(h2!x, 6, 0)
         writef(" DO ")
         prctxtc(h3!x, 6)
         RETURN

 
    CASE s_test:
         writef("TEST ")
         prctxte(h2!x, 6, 0)
         writef(" THEN ")
         prctxtc(h3!x, 6)
         writef(" ELSE ")
         prctxtc(h4!x, 6)
         RETURN
 
    CASE s_loop:
         writef("LOOP")
         RETURN
 
    CASE s_exit:
         writef("EXIT")
         RETURN
 
    CASE s_next:
         writef("NEXT")
         RETURN
 
    CASE s_skip:
         writef("{}")
         RETURN
 
    CASE s_break:
         writef("BREAK")
         RETURN
 
    CASE s_return:
         writef("RETURN")
         RETURN
 
    CASE s_finish:
         writef("FINISH")
         RETURN
 
    CASE s_resultis:
         writef("RESULTIS ")
         prctxte(h2!x, 6, 0)
         RETURN
 
    CASE s_repeatwhile:
    CASE s_repeatuntil:
         prctxtc(h2!x, 6)
         writef(op=s_repeatwhile -> " REPEATWHILE ", " REPEATUNTIL ")
         prctxte(h3!x, 6, 0)
         RETURN
 
    CASE s_repeat:
         prctxtc(h2!x, 6)
         writef(" REPEAT")
         RETURN
 
    CASE s_case:
         writef("CASE ")
         prctxte(h2!x, 6, 0)
         writef(":.. ")
         RETURN
 
    CASE s_default:
         writef("DEFAULT:..")
         RETURN
 
    CASE s_endcase:
         writef("ENDCASE")
         RETURN
 
    CASE s_switchon:
         writef("SWITCHON ")
         prctxte(h2!x, 6, 0)
         writef(" INTO..")
         RETURN
 
    CASE s_for:
         writef("FOR ")
         prctxte(h2!x, 6, 0)
         writef("=")
         prctxte(h3!x, 6, 0)
         writef(" TO ")
         prctxte(h4!x, 6, 0)
         IF h5!x DO { writef(" BY "); prctxte(h5!x, 6, 0) }
         writef(" DO..")
         RETURN
 
    CASE s_seq:
         prctxtc(h2!x, 6)
         writef(";")
         prctxtc(h3!x, 6)
         RETURN
  }
}

AND prctxtd(x, d) BE writef("..")
AND prctxtc(x, d) BE writef("..")

AND wrhexval(n) BE
{ LET lsdig = n & #xFF
  n := n>>8
  IF n DO wrhexval(n)
  writef("%2x", lsdig)
}

AND prctxte(x, d, prec) BE IF x DO
{ LET op = h1!x

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_number: 
                 { LET n = h2!x
                   TEST -1_000_000<=n<=1_000_000
                   THEN writef("%n", n)
                   ELSE { IF n<0 DO
		          { wrch('-')
			    n := -n
			  }
		          writef("#x")
			  wrhexval(n)
			}
                   RETURN
                 } 
    
    CASE s_fnum: 
                 { LET n = h2!x
                   writef("%5.3f", n)
                   RETURN
                 } 
    
    CASE s_flt :   writef("FLT %s", @h3!(h2!x)); RETURN
    CASE s_name:   writef("%s", @h3!x);          RETURN
    CASE s_true:   writef("TRUE");               RETURN
    CASE s_false:  writef("FALSE");              RETURN
    CASE s_query:  wrch('?');                    RETURN

    CASE s_string: 
                 { LET s = @h2!x
                   LET len = s%0
                   wrch('"')
                   FOR i = 1 TO len DO
                   { LET ch = s%i
                     IF i=6 & len>6+8 DO { writef("'"); LOOP }
                     IF i<=6 | i>len-8 DO // First 5 and last 8 chars
                     { SWITCHON ch INTO
                       { CASE '**': writef("****"); LOOP
                         CASE '*"': writef("***""); LOOP
                         CASE '*n': writef("**n");  LOOP
                       }
                       UNLESS 32<=ch<=127 DO ch := '?'
                       wrch(ch)
                     }
                   }
                   wrch('"')
                   RETURN
                 }

  }

  IF d=0 DO { writef("..."); RETURN }

  IF prec>=12 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_fnap:
         prctxte(h2!x, d-1, 11)
         wrch('(')
         prctxte(h3!x, d-1, 0)
         wrch(')')
         RETURN
  }

  IF prec>=11 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_slct:
      {  writes("SLCT ")
         prctxte(h2!x, d-1, 10)
         writes(":")
         prctxte(h3!x, d-1, 10)
         writes(":")
         prctxte(h4!x, d-1, 10)
         RETURN
      }

    CASE s_of:
    CASE s_byteap:
    CASE s_vecap:
         prctxte(h2!x, d-1, 10)
         writes(op=s_of->"::", op=s_byteap->"%", "!")
         prctxte(h3!x, d-1, 10)
         RETURN

    CASE s_float:
    CASE s_fix:
    CASE s_rv:
    CASE s_lv:
         writef(op=s_float->"FLOAT ",
                op=s_fix  ->"FIX ",
                op=s_rv   ->"!",
                            "@")
         prctxte(h2!x, d-1, 10)
         RETURN
  }

  IF prec>=10 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_mul: CASE s_div: CASE s_mod:
         prctxte(h2!x, d-1, 9)
         writef(op=s_mul->"**", op=s_div->"/", " MOD ")
         prctxte(h3!x, d-1, 9)
         RETURN

    CASE s_fmul: CASE s_fdiv: CASE s_fmod:
         prctxte(h2!x, d-1, 9)
         writef(op=s_fmul -> "#**",
                op=s_fdiv -> "#/",
                             "#MOD ")
         prctxte(h3!x, d-1, 9)
         RETURN
  }

  IF prec>=9 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_add:
    CASE s_sub:
         prctxte(h2!x, d-1, 8)
         writef(op=s_add->"+","-")
         prctxte(h3!x, d-1, 8)
         RETURN

    CASE s_fadd:
    CASE s_fsub:
         prctxte(h2!x, d-1, 8)
         writef(op=s_fadd->"#+","#-")
         prctxte(h3!x, d-1, 8)
         RETURN

    CASE s_pos:
    CASE s_neg:
    CASE s_fneg:
    CASE s_fabs:
    CASE s_abs:
         writef(op=s_pos ->"+",
                op=s_neg ->"-",
                op=s_fneg->"#-",
                op=s_fabs->"#ABS ",
                           "ABS ")
         prctxte(h2!x, d-1, 8)
         RETURN
  }

  IF prec>=8 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_eq: CASE s_ne:
         prctxte(h2!x, d-1, 7)
         writef(op=s_eq->"=","~=")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_feq: CASE s_fne:
         prctxte(h2!x, d-1, 7)
         writef(op=s_feq->"#=","#~=")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_ls: CASE s_gr:
         prctxte(h2!x, d-1, 7)
         writef(op=s_ls->"<",">")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_fls: CASE s_fgr:
         prctxte(h2!x, d-1, 7)
         writef(op=s_fls->"#<","#>")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_le: CASE s_ge:
         prctxte(h2!x, d-1, 7)
         writef(op=s_le->"<=",">=")
         prctxte(h3!x, d-1, 7)
         RETURN

    CASE s_fle: CASE s_fge:
         prctxte(h2!x, d-1, 7)
         writef(op=s_fle->"#<=","#>=")
         prctxte(h3!x, d-1, 7)
         RETURN
  }

  IF prec>=7 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_lshift: CASE s_rshift:
         prctxte(h2!x, d-1, 6)
         writef(op=s_lshift->"<<",">>")
         prctxte(h3!x, d-1, 6)
         RETURN
  }

  IF prec>=6 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_not:
         wrch('~')
         prctxte(h2!x, d-1, 5)
         RETURN
  }

  IF prec>=5 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_logand:
         prctxte(h2!x, d-1, 4)
         wrch('&')
         prctxte(h3!x, d-1, 4)
         RETURN
  }

  IF prec>=4 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_logor:
         prctxte(h2!x, d-1, 3)
         wrch('|')
         prctxte(h3!x, d-1, 3)
         RETURN
  }

  IF prec>=3 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE
    CASE s_eqv:
    CASE s_xor:
         prctxte(h2!x, d-1, 2)
         writef(op=s_eqv->" EQV "," XOR ")
         prctxte(h3!x, d-1, 2)
         RETURN

  }

  IF prec>=2 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: ENDCASE

    CASE s_cond:
    CASE s_fcond:
         prctxte(h2!x, d-1, 1)
         writef(op=s_cond -> "->", "#->")
         prctxte(h3!x, d-1, 1)
         writef(",")
         prctxte(h4!x, d-1, 1)
         RETURN
  }

  IF prec>=1 DO { wrch('('); prctxte(x, d, 0); wrch(')'); RETURN }

  SWITCHON op INTO
  { DEFAULT: writef("Op%n", op); RETURN

    CASE s_table:
         writef("TABLE ")
         prctxte(h2!x, d-1, 0)
         RETURN
         
    CASE s_valof:
         writef("VALOF{")
         prctxtc(h2!x, d-1)
         wrch('}')
         RETURN

    CASE s_comma:
         prctxte(h2!x, d-1, 0)
         writef(",")
         prctxte(h3!x, d-1, 0)
         RETURN
  }
}


AND out1(x) BE wrn(x)
 
AND out2(x, y) BE { out1(x); out1(y) }
 
AND out3(x, y, z) BE { out1(x); out1(y); out1(z) }
 
AND out4(x, y, z, t) BE { out1(x); out1(y); out1(z); out1(t) }
 
AND outstring(s) BE FOR i = 0 TO s%0 DO out1(s%i)

AND outcomment(s, a, b, c, d) BE UNLESS nocomments DO
{ LET prevout = output()
  LET ramstream = findoutput("RAM:")
  selectoutput(ramstream)
  writef(s, a, b, c, d)
  selectoutput(prevout)

  { LET buf = scb_buf!ramstream
    LET pos = scb_pos!ramstream
    
    out2(s_comment, pos)
  
    FOR i = 0 TO pos-1 DO
    { LET ch= buf%i
      out1(ch)
    }
    endstream(ramstream)
  }
}

