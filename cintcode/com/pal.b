/*
########## UNDER EARLY STAGE OF DEVELOPMENT #############

This is a compiler and interpreter for the language PAL
implemented in BCPL, based on the version for the IBM 360
at MIT last modified by R Mabee in June 1970.

Substatially modified to run under modern Cintcode BCPL
(c) Martin Richards 04 Mar 2024

Usage:

pal  "PROG/A,TO=-o/K,TOKENS=-l/S,TREE=-p/S,CODE=-c/S,TRACE=-t/S"

   PROG   gives the filename of the PAL program to run, eg test.pal
-o TO     gives the filename of the output
-l TOKENS is a switch to test the lexical analyser
-p TREE   causes the parse tree to be output
-c CODE   outputs the compiled blackboard evaluator code
-t TRACE  Traces the execution of the blackboard evaluator.

History

25/03/2024
Still working on the interpreter in pal.b based on xpal70.

25/02/2024
Started modifying this compiler to make it follow the syntax and
POCODE of the Pal70 compiler whose original compiler listing is in
PAL/pal/docs/pal70-mabee-17jun70.pdf and the xpal files in PAL.

08/07/2010
Started to modify lex and syn to agree with the PAL syntax specified
in Appendix 2.1 (dated 02/17/68) with the following minor extensions.

The operators ~=, <= and >= are included.
( and [ are synonyms as are ) and ].
-> and -* are synonyms.
~ and not are synonyms.

14/06/2010
Lex more or less complete, now working on the syntax analyser.

09/06/2010
Started re-implementation of PAL based on VSPL.

*/


GET "libhdr"
 
MANIFEST {
// Selectors
h1=0; h2; h3; h4; h5; h6; h7

// Lexical tokens and parse tree operators
s_and=1
s_apply
s_ass
s_aug
s_bar
s_colon
s_comma
s_cond
s_def
s_div
s_do
s_dot
s_dummy  // Not in pal70
s_else   // Not in pal70
s_eof
s_eq
s_false  // Not in pal70
s_ge
s_goto
s_gr
s_if
s_ifnot
s_ifso
s_in
s_int
s_jj
s_lab
s_lambda
s_le
s_let
s_logand
s_logor
s_lparen
s_ls
s_lsect // Not in pal70
s_minus
s_mpt
s_mult
s_name
s_ne
s_neg
s_nil
s_noshare
s_not
s_paren
s_percent
s_pling   // '!' Not in pal70
s_plus
s_pos
s_power
s_real
s_rec
s_res
s_rparen
s_rsect // Not in pal70
s_sbra  // Not in pal70
s_seq
s_sket  // Not in pal70
s_stringconst
s_sys   // Not in pal70
s_test
s_then
s_true   // Not in pal70
s_tuple
s_valdef
s_valof
s_where
s_while
s_within


// POCODE operators
i_apply
i_aug
i_blocklink
i_decllabel
i_declname
i_declnames
i_div
i_dummy   // Not in Xpal70
i_eq
i_false   // Not in Xpal70
i_finish
i_formClosure
i_formLvalue
i_formRvalue
i_ge
i_goto
i_gr
i_halt    // Not in Xpal70
i_initname
i_initnames
i_jj      // Not in xpal70
i_jump
i_jumpF;
i_le
i_loadE
i_loadF
i_loadGuess
i_loadJ   // Not in Xpal70
i_loadL
i_loadN
i_loadR
i_loadS
i_logand
i_logor
i_lose1
i_ls
i_members
i_minus
i_mult
i_name
i_ne
i_neg
i_nil
i_norestart
i_not
i_number
i_okrestart
i_plus
i_pos
i_power
i_res
i_reslink
i_restart
i_restoreE1
i_result  // Not in xpal70
i_return
i_rvrestart
i_save
i_setlabEs
i_setup
i_stringconst
i_sys     // Not in Xpal70
i_testEmpty;
i_true    // Not in Xpal70
i_tuple   // Not in Xpal70
i_update

//Integer; Lab; Param; Equ // Pocode operators only used
                           // by xpal70 loader.

t_basicfn
t_closure
t_dummy;
t_env
t_false
t_guess
t_int
t_jj
t_label
t_loadGuess
t_lvalue
t_nil
t_nils;
t_real
t_stack
t_string
t_sys
t_true
t_tuple

// Translation symbols
m_val=0; m_ref  // L and R mode


bytemax=255
}


GLOBAL { 
rec_p:ug; rec_l; fin_p; fin_l
fatalerr; synerr; trnerr; errcount; errmax
progstream; tostream
mk1; mk2; mk3; mk4; mk5; mk6
newvec; treep; treevec
optTokens; optTree; optCode; optTrace

// Globals used in LEX
chbuf; charv; ch; rch; lex
tree
token; decval; fltval; exponent; wordnode
nilnode; truenode; falsenode; dummynode; mptnode
wrchbuf; chcount; lineno
dsw; declsyswords; namestart; nametable; lookupword
rdnumber; rdstrch; rdtag

// Globals used in SYN
checkfor; rdprog
rdnamelist; rname
rdnbdef; rbdef; rndef; rdef
formtree; plist
rnexp; rexp; rnbexp; rbexp
rncom; rcom; rbcom

// Globals used in TRN and the interpreter
 
trnext:300; trprog; trdef; trans
findlabels; translabels; transrhs
loaddefinee; declguesses; initnames; transscope
mapb; mapf; length; upssp
trcom; decldyn
declstatnames; checkdistinct; addname; cellwithname
trdecl; jumpcond
assign; load; fnbody; loadlist; transname
dvec; dvece; dvecp; dvect
comline; procname; resultlab; ssp; msp
outf; outfname; outnamepos; outstring; outfv; outfn; outfsl
outfnn; outfl; outfs; outentry
outlab; outlabset; outvar; outstatvec
opstr; Operands

mem; memt
codev; codep; codet

datavupb
datav
palsxv
prevdatav

// Runtime state
pc      // Abs address of the next instruction in codev
oldc    // Abs address of a location in codev or zero
sp      // Abs address to top of 2nd from top of stack

// Abs addresses of locations within datav
env     // Address rel to datav to [5, Env, link, name, value]
rega    // Address rel to datav or zero
regb    // Address rel to datav or zero
stack   // Address rel to datav

// Pointers to nodes in datav held in runtime nodes such
// as stacks are relative to datav (not absolute).

count
prstate
prvalue
printa
printb

labv; refv; labmax; putc; putd; putref
setlab; setlabval; nextlab; labnumber; resolvelabels
execpal; printf
writechar

gclimit     // call the garbage collector when h1!data>gclimit
garbcollect // If garbage collection does not recover sufficient
            // space increase gclimit somewhat.
gc

// PAL runtime globals
retcode

//stackp
dummyrv
nilrv
nilsrv
truerv
falserv
guessrv

list     // Allocate runtime n0des of size 1 to 7
node     // Allocate runtime n0des of any size

storage; storaget
xpal
control
strvupb; strv // Self expanding vector of distinct variable names
              // strv is zero or strv!0 is the subscript of the
	      // last element in strv. Items in strv are of the form
	      // [ legth in words, <packed string> ] eg
	      // p:   3          Item length in words
	      // p+1: 43424104   BCPL string "ABCD"
	      // p+2: 00000044
	      // p+3:            Start of next item
	      // The last item has a length field of zero
sxvstr        // This will point to strvupb

str2sxvstr    // Find or insert a variable nameintp sxvstr
              // The result is the position in strv of the
	      // packed characters.
sxvpush

codefilep
namechain
mapliblist
parv
reft
r_finish
listv; listp; listt; listl
linev; linep; linet
gcmark
lookupno
lookupnovar
nset
time_exceeded
timeovfl
maxct
terminate
codefile
refp
stof
stackwarning
gcdbg
writex
dataflag
lvch
readch
q
errlvdbg
edbg
errdbg
errokdbg
done

f_apply
f_atom
f_aug
f_blocklink
f_conc
f_cton
f_decllabel
f_declname
f_declnames
f_decllib
f_diagnose
f_div
f_dummy
f_eq
f_false
f_finish
f_formClosure
f_formLvalue
f_formRvalue
f_ftos
f_ge
f_goto
f_gr
f_halt
f_initname
f_initnames
f_isdummy
f_isenvironment
f_isfunction
f_islabel
f_isnumber
f_isreal
f_isstring
f_istruthvalue
f_istuple
f_itoc
f_itor
f_jump
f_jumpF
f_lastfn
f_le
f_length
f_libname
f_loadE
f_loadF
f_loadF   // Load real
f_loadGuess
f_loadJ
f_loadL
f_loadN   // Load int
f_loadR
f_loadS
f_logand
f_logor
f_lookupinE
f_lose1
f_ls
f_members
f_minus
f_mult
f_name
f_ne
f_neg
f_nil
f_not
f_ntor
f_null
f_number
f_plus
f_pos
f_power
f_print
f_rdchar
f_reslink
f_restoreE1
f_result
f_return
f_rtoi
f_rton
f_rvrestart
f_save
f_saveenv
f_setlabEs
f_setup
f_share
f_stem
f_stern
f_ston
f_stringconst
f_sys
f_table
f_testEmpty
f_true
f_tuple
f_update
f_userpage

finishLoc
restartLoc
rvrestartLoc
okrestartLoc
norestartLoc
startLoc


save
push
pop
lvofname
pushlva
mklvnode
nextlv11
blocklink
error
error1
errflag
testbools2
nameres
return
testnumbs2
fpower
fmult
fdiv
fadd
fsub
fumin
equal
feq
fls
fle
fge
fgr
floterr
restart
tupledepth
printa
//writechar
restartc
rvrestart
okrestart
errorlv
////////
errct
maxerr
nil
formLvalue
terminate1
restartpc
xpend; xpendlevel

//glob #####
}

MANIFEST {                         //  Selectors
nametablesize = 541
c_tab         =   9
c_newline     =  10
}

LET abort(n) BE writef("*nabort(%n) called*n", n)

LET start() = VALOF
{ LET treesize = 0
  AND codesize = 0
  AND argv = VEC 50
  AND argform =
        "PROG/A,TO=-o/K,TOKENS=-l/S,TREE=-p/S,CODE=-c/S,TRACE=-t/S"
  LET stdout = output()

  tupledepth := 3
  errmax   := 2
  errcount := 0
  fin_p, fin_l := level(), fin

  treevec, labv, refv, mem := 0, 0, 0, 0
  progstream := 0 // The pal program
  tostream := 0
   
  writef("*nPAL (12 Mar 2024)*n")
 
  IF rdargs(argform, argv, 50)=0 DO fatalerr("Bad arguments*n")

  treesize := 10000
  codesize := 50000

  progstream := findinput(argv!0)      // PROG  The Pal prgram

  UNLESS progstream DO fatalerr("Trouble with file %s*n", argv!0)

  selectinput(progstream)
 
  IF argv!1                            // TO      -o
  DO { tostream := findoutput(argv!1)
       IF tostream=0 DO
         fatalerr("Trouble with code file %s*n", argv!1)
     }

  optTokens := argv!2                  // TOKENS  -l
  optTree   := argv!3                  // TREE    -p
  optCode   := argv!4                  // CODE    -c
  optTrace  := argv!5                  // TRACE   -t

  treevec := getvec(treesize)

  codev   := getvec(codesize)  // For the compiled Pocode
  codep   := 0                 // It should be replaced by
  codet   := codesize          // a self expanding vector

  strvupb, strv := 0, 0  // Initialise the sxvstr selfexpanding vector.
  sxvstr := @ strvupb
  sxvpush(sxvstr, 0)
  
  IF FALSE DO
  { LET p = 0
    writef("Testing str2sxvstr*n")
    p := str2sxvstr("ABCD")
    writef("%i3 '%s'*n", p, strv+p)
    p := str2sxvstr("DEF")
    writef("%i3 '%s'*n", p, strv+p)
    p := str2sxvstr("ABCD")
    writef("%i3 '%s'*n", p, strv+p)
    p := str2sxvstr("XYZ")
    writef("%i3 '%s'*n", p, strv+p)
    RESULTIS 0
  }

  // Initialise the Pal runtime data space

  datavupb := 0  // Self expanding vector control block
  datav    := 0  // Only changed by calls of node or list
  palsxv   := @datavupb
  prevdatav := 0

  labv := getvec(2000)
  refv := getvec(2000)
  
  labmax := 2000

  UNLESS treevec & codev & labv & refv DO
     fatalerr("Insufficient memory*n")
   
  UNLESS tostream DO tostream := stdout
  selectoutput(tostream)

  chbuf := getvec(64/bytesperword)
  FOR i = 0 TO 63 DO chbuf%i := 0
  chcount, lineno := 0, 1
  rch()
 
  treep := treevec + treesize

  tree := formtree()              // Perform Syntax Analysis

  IF optTokens GOTO fin

  IF optTree DO { writes("*nParse Tree*n*n")
                  plist(tree, 0, 20)
                  newline()
		  //abort(1000)
                }
  
  IF errcount GOTO fin

  FOR i = 0 TO codet DO codev!i := 0

  trprog(tree)                    // Translate the tree

  IF errcount GOTO fin

  writef("*nStarting the interpreter*n*n")

  xpendlevel := level()

  sp := 0
  pc := 0
  env := 0
  stack := 0
  oldc := 0
  
  execpal()   // Execute Pocode instructions
  IF retcode DO writef("Return code %n*n", retcode)
  writef("*nInstructions executed: %n*n", count)
   
xpend:
fin:
  IF treevec       DO freevec(treevec)
  IF chbuf         DO freevec(chbuf)
  IF mem           DO freevec(mem)
  IF labv          DO freevec(labv)
  IF refv          DO freevec(refv)
  IF progstream    DO endstream(progstream)
  IF tostream UNLESS tostream=stdout DO endstream(tostream)

  selectoutput(stdout)
  RESULTIS errcount=0 -> 0, 20
}

AND str2sxvstr(str) = VALOF
{ // str holds the name for PAL variable or a string constant
  // All strings in sxvstr are distinct.
  LET p = 1
  LET upb = str%0/bytesperword // upb of str in words
  //writef("*nstr2sxvstr: str=%s upb=%n p=%n strv!p=%n*n", str, upb, p, strv!p)
  //FOR i = 0 TO upb DO writef(" %x8", str!i)
  //newline()
  //abort(1002)
  WHILE strv!p DO // Item size
  { LET s = strv+p+1 // The next string in strv to inspect.
    LET found = TRUE
    //writef("item size=%n str=%s s=%s p=%n*n", strv!p, str, s, p) 
    FOR i = 0 TO upb UNLESS str!i=s!i
    { found := FALSE
      BREAK
    }
    IF found DO
    { //writef("Name found at p=%n*n", p+1)
      RESULTIS p+1 // Position of matching string
                          // relative to strv
    }
    // Not found
    p := p+strv!p
    //writef("Item ar p=%n does not match*n", p)
  }
  // str not found so add it
  //writef("match not found in sxvstr p=%n upb=%n %s strv=%n*n",
  //        p, upb, str, strv)
  //writef("sxvstr=%n -> [ %n %n ]*n", sxvstr, sxvstr!0, sxvstr!1)
  //abort(1001)
  strv!p := upb+2 // Size of the new item
  FOR i = 0 TO upb DO sxvpush(sxvstr, str!i)
  sxvpush(sxvstr, 0)
  //writef("sxvstr data after insertion is*n")
  //FOR i = 0 TO strv!0 DO
  //{ IF i MOD 5 = 0 DO newline()
  //  writef(" %x8", strv!i)
  //}
  //newline()
  //writef("After insertion: sxvstr=%n -> [ %n %n ]*n",
  //        sxvstr, sxvstr!0, sxvstr!1)
  //writef("Returning p+1=%n*n", p+1)
  //abort(1004)
  RESULTIS p+1
}

LET lex() BE
{ //writef("lex: ch=%n '%c'*n", ch, ch>=32 ->ch, '?')
  SWITCHON ch INTO
  {       
    CASE '*p': CASE '*n':
      lineno := lineno + 1
    CASE '*c': CASE '*t': CASE '*s':
      rch()
      LOOP

    CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
    CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
      rdnumber()
      //writef("lex: token=%n decval=%n fltval=%8.3f*n",
      //        token, decval, fltval)
      RETURN
 
    CASE 'a':CASE 'b':CASE 'c':CASE 'd':CASE 'e':
    CASE 'f':CASE 'g':CASE 'h':CASE 'i':CASE 'j':
    CASE 'k':CASE 'l':CASE 'm':CASE 'n':CASE 'o':
    CASE 'p':CASE 'q':CASE 'r':CASE 's':CASE 't':
    CASE 'u':CASE 'v':CASE 'w':CASE 'x':CASE 'y':
    CASE 'z':
    CASE 'A':CASE 'B':CASE 'C':CASE 'D':CASE 'E':
    CASE 'F':CASE 'G':CASE 'H':CASE 'I':CASE 'J':
    CASE 'K':CASE 'L':CASE 'M':CASE 'N':CASE 'O':
    CASE 'P':CASE 'Q':CASE 'R':CASE 'S':CASE 'T':
    CASE 'U':CASE 'V':CASE 'W':CASE 'X':CASE 'Y':
    CASE 'Z':
      token := lookupword(rdtag())
      //IF token=s_name DO
      //  writef("lexX: %s %x8 %x8 %s*n",
      //          opstr(token), h3!wordnode, h4!wordnode, @h3!wordnode)
      // wordnode -> [ op, hashchain, <packed characters> ]
      RETURN

    CASE '*'': // A string constant
              { LET len = 0
	        LET upb = 0
                rch()
 
                UNTIL ch='*'' DO
                { IF len=255 DO synerr("String constant too long")
                  len := len + 1
                  charv%len := rdstrch()
                }
                charv%0 := len
		upb := len / bytesperword

		{ len := len+1  //Pad with zero bytes
		  UNLESS len MOD bytesperword BREAK
		  charv%len := 0
		} REPEAT

                wordnode := newvec(upb+2)
                h1!wordnode := s_stringconst
                FOR i = 0 TO upb DO wordnode!(i+1) := charv!i
                token := s_stringconst
		// wordnode -> [ Stringconst, <packed characters> ]
		//writef("Stringconst: '%s'*n", @h2!wordnode)
		//FOR i = 0 TO upb DO writef(" %x8", wordnode!(i+1))
		//newline()
                BREAK
              }
 
    CASE '{': token := s_lsect;     BREAK // Not in pal70
    CASE '}': token := s_rsect;     BREAK // Not in pal70
    CASE '[': token := s_sbra;      BREAK // Not in pal70
    CASE ']': token := s_sket;      BREAK // Not in pal70
    CASE '(': token := s_lparen;    BREAK
    CASE ')': token := s_rparen;    BREAK 
    CASE '%': token := s_percent;   BREAK 
    CASE '+': token := s_plus;      BREAK
    CASE ',': token := s_comma;     BREAK
    CASE '&': token := s_logand;    BREAK
    CASE '|': token := s_bar;       BREAK
    CASE '!': token := s_pling;     BREAK // Not in pal70
    CASE '=': token := s_eq;        BREAK // This was valdef in pal70
    CASE '^': token := s_power;     BREAK // Not in pal70
    CASE ';': token := s_seq;       BREAK
    CASE '$': token := s_noshare;   BREAK
    CASE '.': token := s_dot;       BREAK // Not in pal70
 
    CASE '**':
      rch()
      IF ch='**' DO { token := s_power;  BREAK }
      token := s_mult
      RETURN

    CASE '/':
      rch()
      IF ch='/' DO
      { rch() REPEATUNTIL ch='*n' | ch=endstreamch
        LOOP
      }
      token := s_div
      RETURN
 
    CASE '<':   rch()
                IF ch='=' DO { token := s_le;  BREAK }
                token := s_ls
                RETURN

    CASE '>':   rch()
                IF ch='=' DO { token := s_ge;  BREAK }
                token := s_gr
                RETURN

    CASE '~':   rch()
                IF ch='=' DO { token := s_ne;  BREAK }
                token := s_not
                RETURN

    CASE '-':   rch()
                IF ch='>' | ch='**' DO { token := s_cond; BREAK }
                token := s_minus
                RETURN

    CASE ':':   rch()
                IF ch='=' DO { token := s_ass;  BREAK }
                token := s_colon
                RETURN
 
    DEFAULT:    UNLESS ch=endstreamch DO
                { LET badch = ch
                  ch := '*s'
                  synerr("Illegal character %x2", badch)
                }
                token := s_eof
                RETURN
  } REPEAT
 
  rch()
}
 
LET lookupword(word) = VALOF
{ // word is a BCPL string padded with zero bytes
  LET len, i = word%0, 0
  LET upb = len / bytesperword
  LET hashval = 0
  //writef("lookupword: word=%s %x8 %x8*n", word, word!0, word!1)
  //abort(6666)
  //writef("lookupword: word=%n=%s len=%n upb=%n*n", word, word, len, upb)
  //abort(1011)
  FOR i = 0 TO upb DO hashval := (13*hashval + word!i) / 3 MOD nametablesize
  IF hashval<0 DO abort(999)
  wordnode := nametable!hashval
 
  WHILE wordnode & i<=upb TEST (@h3!wordnode)!i=word!i
                          THEN i := i+1
                          ELSE wordnode, i := h2!wordnode, 0
  IF wordnode=0 DO
  { LET upb = len/bytesperword
    wordnode := newvec(upb+2)
    h1!wordnode, h2!wordnode := s_name, nametable!hashval
    FOR i = 0 TO upb DO (@h3!wordnode)!i := word!i
    nametable!hashval := wordnode
  }
  //writef("lookupword: word=%s => %n s_name=%n wordnode=%n*n",
  //        word, h1!wordnode, s_name, wordnode)
  //abort(3001)
  RESULTIS h1!wordnode
}
 
AND dsw(word, tok) BE
{ //IF tok=69 DO abort(1009)
  lookupword(word)
//IF tok=69 DO abort(1010)
  h1!wordnode := tok
  //writef("dsw: entered, word=%s tok=%n wordnode=%n*n", word, tok, wordnode)
  //abort(998)
}
 
AND declsyswords() BE
{ 
  dsw("and", s_and)
  dsw("aug", s_aug)
  dsw("def", s_def)
  dsw("do", s_do)
  dsw("dummy", s_dummy)
  dummynode := wordnode
  dsw("else", s_else) // Not in pal70
  dsw("eq", s_eq)
  dsw("false", s_false)
  falsenode := wordnode
  dsw("fn", s_lambda)
  dsw("ge", s_ge)
  dsw("goto", s_goto)
  dsw("gr", s_gr)
  dsw("if", s_if)
  dsw("ifnot", s_ifnot)
  dsw("ifso", s_ifso)
  dsw("in", s_in)
  dsw("jj", s_jj) // Not in pal70
  dsw("le", s_le)
  dsw("let", s_let)
  dsw("ll", s_lambda) // Not in pal70
  dsw("logand", s_logand) // Not in pal70
  dsw("ls", s_ls)
  dsw("ne", s_ne)
  dsw("nil", s_nil)
  nilnode := wordnode
  dsw("not", s_not)
  dsw("or", s_logor)
  dsw("rec", s_rec)
  dsw("res", s_res)
  dsw("sys", s_sys) // Not in pal70
  dsw("test", s_test)
  dsw("then", s_then) // Not in pal70
  dsw("true", s_true)
  truenode := wordnode
  dsw("valof", s_valof)
  dsw("where", s_where)
  dsw("while", s_while)
  dsw("within", s_within)
  //abort(1000)
} 
 
LET rch() BE
{ ch := rdch()
  chcount := chcount+1
  chbuf%(chcount&63) := ch
}
 
AND wrchbuf() BE
{ writes("*n...")
  FOR p = chcount-63 TO chcount DO
  { LET k = chbuf%(p&63)
    IF 0<k<255 DO wrch(k)
  }
  newline()
}

AND rdnumber() BE
{ // Read an integer or floating point constant
  // setting token to s_number with the integer value in decval
  // or s_fnum with the floating point value in fltval which will
  // be in IEE standard 32 or 64 bit format depending on the BCPL
  // word length of the compiler.
  // The strategy is to simultaneously construct both the integer
  // and floating point values. It stops constructing the integer
  // value after reading a decimal point or e, ie when the constant
  // is known to be floating point. Care is needed with eg 123..
  // which is s_number followed by s_range.
  LET pos      = 0    // Number of integer and fractional digits
                      // in the number.
  LET sigpos   = 0    // Position of the last significant digit
  LET pointpos = 0    // Position of the digit just left of the
                      // decimal point
  LET FLT flt0  = 0.0
  LET FLT flt1  = 1.0
  LET FLT flt10 = 10.0
//abort(1001)  
  token := s_int      // Until '.' or 'e' encountered
  decval, exponent, fltval := 0, 0, flt0

  // Ignore spaces
  WHILE ch='*s' | ch='*t' DO rch()

  // A number must start with a digit.
  UNLESS '0'<=ch<='9' DO synerr("Bad number")

  WHILE '0'<=ch<='9' | ch='_' | ch='.' DO
  { // Deal with digits before e, if any.
    //writef("ch=%c pos=%n token=%n decval=%i4 exponent=%n*n",
    //        ch, pos, token, decval, exponent)
    SWITCHON ch INTO
    { DEFAULT: BREAK // ch is either e, E or terminates the number.

      CASE '0': CASE '1': CASE '2': CASE '3': CASE '4': 
      CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
      { LET x = sys(Sys_flt, fl_mul, fltval, flt10)  // = 10 * fltval
        pos := pos+1                 // Increment count of digits
        IF token=s_int DO pointpos := pos

        decval := 10*decval + ch-'0' // Accumulate the integer value
        // decval might overflow if too many digits are given.
	
        IF sys(Sys_flt, fl_eq, x, sys(Sys_flt, fl_add, x, flt1)) ENDCASE

        // fltval * 10 + 1 is not equal to fltval * 10, so
        // the digit is significant
        // Perform fltval := x + FLOAT(ch-'0') and increment sigpos.
        fltval := sys(Sys_flt,
                      fl_add, x, sys(Sys_flt, fl_float, ch-'0'))
        sigpos := sigpos+1
        ENDCASE
      }

      CASE '.':
      { LET k = rdch()
        unrdch() // Unread the character after the dot.
        IF k='.' DO
	{ // Found .. which is s_range, so the dot is not part of a
	  // floating point number.
	  RETURN   // Return with token=s_int
	}
        IF token=s_real DO synerr("Two decimal points in a number")
        token := s_real
        ENDCASE
      }
      
      CASE '_':  // Ignore underlines in numbers.
        ENDCASE
    }
    rch()
  }

//sawritef("rdnumber: token=%n decval=%n fltval=%13.1e *
//         *pos=%n sigpos=%n pointpos=%n*n",
//          token, decval, fltval, pos, sigpos, pointpos)

  IF ch='e' | ch='E' DO
  { LET expneg = FALSE
    token := s_real
    rch()
    IF ch='-' DO { expneg := TRUE; rch() }
    WHILE '0'<=ch<='9' | ch='_' DO
    { UNLESS ch='_' DO exponent := 10*exponent + ch-'0'
      rch()
    }
    IF expneg DO exponent := -exponent
  }

  IF token=s_int DO
  { // There was no decimal point or e so leave token=s_int
    // and the integer value in decval.
    RETURN
  }

  // token is s_real

//sawritef("*nrdnumber: making fnumber fltval=%13.1e *
//         *exponent=%n sigpos=%n, pointpos=%n*n",
//          fltval, exponent, sigpos, pointpos)
  // Correct the exponent
  exponent := exponent + pointpos - sigpos

  UNLESS -127 <= exponent <= 127 DO
    synerr("Floating point exponent out of range")

  // Set fltval to fltval x 10^exponent
  TEST exponent>=0
  THEN FOR i = 1 TO exponent DO
         fltval := sys(Sys_flt, fl_mul, fltval, flt10)
  ELSE FOR i = -1 TO exponent BY -1 DO
         fltval := sys(Sys_flt, fl_div, fltval, flt10)
//sawritef("*n=> fltval=%13e*n", fltval)

  // fltval is a floating point number of the same size as
  // the BCPL word length.
}


AND rdtag() = VALOF
{ LET len = 0
  WHILE 'a'<=ch<='z' | 'A'<=ch<='Z' | '0'<=ch<='9' |  ch='_' DO
  { len := len+1
    IF len>255 DO synerr("Name too long")
    charv%len := ch
    rch()
  }
  charv%0 := len

  // Pad the string with zero bytes
  { len := len+1
  //writef("rdtag: padding with zero bytes, len=%n*n", len)
    UNLESS len MOD bytesperword BREAK
    charv%len := 0 // Pad with zero bytes
  } REPEAT

  RESULTIS charv
}

AND rdstrch() = VALOF
{ LET res = ch
  IF ch='*n' | ch='*p' DO
  { lineno := lineno+1
    synerr("Unescaped newline character")
  }
  IF ch='**' DO
  { rch()
    SWITCHON ch INTO
    { DEFAULT:   synerr("Bad string or character constant")
      CASE '*'': CASE '"':  res := ch;     ENDCASE // Not in pal70
      CASE 't':  CASE 'T':  res := '*t';   ENDCASE
      CASE 's':  CASE 'S':  res := '*s';   ENDCASE
      CASE 'n':  CASE 'N':  res := '*n';   ENDCASE
      CASE 'b':  CASE 'B':  res := '*b';   ENDCASE // Not in pal70
    }
  }
  rch()
  RESULTIS res
}

LET newvec(n) = VALOF // Not used for runtime data
{ // n is the upb of the new vector
  treep := treep - n - 1;
  IF treep<=treevec DO fatalerr("More workspace needed")
  RESULTIS treep
}
 
AND mk1(a) = VALOF
{ LET p = newvec(0)
  p!0 := a
  RESULTIS p
}
 
AND mk2(a, b) = VALOF
{ LET p = newvec(1)
  p!0, p!1 := a, b
  RESULTIS p
}
 
AND mk3(a, b, c) = VALOF
{ LET p = newvec(2)
  p!0, p!1, p!2 := a, b, c
  RESULTIS p
}
 
AND mk4(a, b, c, d) = VALOF
{ LET p = newvec(3)
  p!0, p!1, p!2, p!3 := a, b, c, d
  RESULTIS p
}
 
AND mk5(a, b, c, d, e) = VALOF
{ LET p = newvec(4)
  p!0, p!1, p!2, p!3, p!4 := a, b, c, d, e
  RESULTIS p
}
 
AND mk6(a, b, c, d, e, f) = VALOF
{ LET p = newvec(5)
  p!0, p!1, p!2, p!3, p!4, p!5 := a, b, c, d, e, f
  RESULTIS p
}
 
AND formtree() = VALOF
{ LET res = 0
  rec_p, rec_l := level(), recover
//writef("formtree: entered*n")
  charv := newvec(256/bytesperword)     
  nametable := newvec(nametablesize)
  UNLESS charv & nametable DO fatalerr("More workspace needed")
  FOR i = 0 TO nametablesize DO nametable!i := 0
//writef("formtree: calling declsyswords*n")
  declsyswords()
  mptnode := mk1(s_mpt)
//writef("formtree: calling lex*n")

  lex()

  IF optTokens DO            // For debugging lex.
  { //abort(1000)
    writef("token = %i3 %s", token, opstr(token))
    IF token=s_int    DO writef("       %n",   decval)
    IF token=s_real   DO writef("      %5.3f", fltval)
    IF token=s_name   DO writef("      %s",    charv)
    IF token=s_stringconst DO
    { writef("    *'")
      FOR i = 1 TO charv%0 SWITCHON charv%i INTO
      { DEFAULT:   wrch(charv%i); ENDCASE
        CASE '*'': writes("**'"); ENDCASE
        CASE '*n': writes("**n"); ENDCASE
        CASE '*p': writes("**p"); ENDCASE
        CASE '*t': writes("**t"); ENDCASE
        CASE '*b': writes("**b"); ENDCASE
      }
      writef("*'")
    }
    newline()
    IF token=s_eof DO
    { abort(999)
      RESULTIS 0
    }
    lex()
  } REPEAT

recover:
  // lex has already been called.
  res := rdprog()
  UNLESS token=s_eof DO fatalerr("Incorrect termination")
  RESULTIS res
}
 
AND fatalerr(mess, a) BE
{ writef("*nFatal error:  ")
  writef(mess, a)
  writes("*nCompilation aborted*n")
  errcount := errcount+1
  longjump(fin_p, fin_l)
}

AND synerr(mess, a) BE
{ writef("*nError near line %n:  ", lineno)
  writef(mess, a)
  wrchbuf()
  errcount := errcount+1
  IF errcount >= errmax DO fatalerr("Too many errors")

  // Skip the rest of the input line 
  UNTIL ch='*n' | ch=endstreamch DO rch()
  lex()

  longjump(rec_p, rec_l)
}

LET checkfor(tok, mess) BE
{ UNLESS token=tok DO synerr(mess)
  lex()
}

LET rdprog() = VALOF
{ // P -> def D0 .. def D0 in C0 eof |
  //      C0 eof

  LET a, ln = 0, lineno

  IF token=s_def DO
  { LET a = rnbdef()
    IF token=s_def RESULTIS list(4, s_def, a, rdprog())
    checkfor(s_in, "'in' expected after some 'defs'")
    RESULTIS list(4, s_def, a, rncom(0))
  }
  RESULTIS rcom(0)
}

AND rnbdef(n) = VALOF
{ lex()
  RESULTIS rbdef(n)
}

AND rbdef(n) = VALOF
{ // BD -> N,...,N = E
  //       N BV...BV = E
  //       ( D )
  //       rec D
  LET op, ln = token, lineno

  SWITCHON op INTO
  { DEFAULT:
  writef("rbdef:op=%s*n",opstr(op))
      synerr("Bad definition, name, rec or '(' expected")

    CASE s_name:
      { LET name = rname()
        ln := lineno

        IF token=s_comma DO
        { // Must be a simultaneous definition
          // N ,..., N = C0
          LET names = rdnamelist(name)
          checkfor(s_eq, "Bad definition")
          RESULTIS mk4(s_valdef, names, rcom(0), ln)
        }

        IF token=s_eq RESULTIS mk4(s_valdef, name, rncom(0), ln)

        // We have a name not followed by a comma
	
        { // Must be a function definition
          // N BV ... BV = C0
          LET v = VEC 50
          AND i, b = 0, ?
          WHILE i<=50 DO
          { UNLESS token=s_lparen | token=s_name BREAK
            v!i := rbv() // Read a name or list of names
	                 // enclosed on parens
            i := i+1
          }
          UNLESS i~=0 & token=s_eq DO synerr("Bad definition")
          b := rncom(0) // Read the function body
          WHILE i>0 DO
          { i := i-1
            b := mk4(s_lambda, v!i, b, ln) // Form lambda expressions
          }
          RESULTIS mk4(s_valdef, name, b, ln)
        }
      }

    CASE s_lparen:
      { LET a = rndef(0)
        checkfor(s_rparen, "Bad definition")
        RESULTIS a
      }

    CASE s_sbra:
      { LET a = rndef(0)
        checkfor(s_sket, "Bad definition")
        RESULTIS a
      }

    CASE s_lsect:      // '{'
      { LET a = rndef(0)
        checkfor(s_rsect, "Bad definition")
        RESULTIS a
      }

    CASE s_rec:
      UNLESS n=0 DO synerr("Redundant 'rec'")
      RESULTIS mk3(s_rec, rnbdef(2), ln)
  }
}

AND rndef(n) = VALOF { lex(); RESULTIS rdef(n) }

AND rdef(n) = VALOF
{ // D -> D and D
  //      D within D
  //      BD
  LET a = rbdef(0)
  LET b = 0

  { LET op, ln = token, lineno

//sawritef("rdef: op=%s ln=%n*n", opstr(op), ln)
    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a

      CASE s_and:
        IF a=0 DO synerr("Definition missing before 'and'")
        IF n>=6 RESULTIS a
        { LET i = 0
          LET v = VEC 100 // To the second and subsequent definitions
	                  // a is the first definitions
          WHILE token=s_and DO
          { i := i+1
	    v!i := rnbdef(0)
          }
	  //i is the number of additional definistions
          b := a
          a := newvec(i+2) // Room for [ And, n, d1,...,dn ]
          a!0, a!1, a!2 := s_and, i+1, b // b is the first definition
	                                 // i+1 is the total number
					 // of definitions in the
					 // and construct.
          FOR j = 1 TO i DO a!(j+2) := v!j
	  //FOR j = 0 TO i+2 DO writef(" %n ", a!j)
	  //newline()
	  //abort(1255)
          LOOP
        }

      CASE s_within:
        IF a=0 DO synerr("Definition missing before 'within'")
        IF n>=3 RESULTIS a
        a := mk4(s_within, a, rndef(0), ln)
        LOOP
    }
  } REPEAT
}

AND rbv() = VALOF
{ // Only called when token is Name or Lparen
  LET a = ?
  IF token=s_name RESULTIS rname()
  checkfor(s_lparen, "'(' expected")
  IF token=s_rparen DO
  { lex()
    RESULTIS mptnode
  }
  a := rdnamelist(0)
  checkfor(s_rparen, "Bad bound variable list")
  RESULTIS a
}

AND rdnamelist(n) = VALOF
{ LET a, b, i, ln = 0, n, 1, lineno
  LET v = VEC 100
  IF n=0 DO
  { UNLESS token=s_name DO
      synerr("Bad name list")
    b := rname()
  }
  UNLESS token=s_comma RESULTIS b
  WHILE token=s_comma DO
  { lex()
    UNLESS token=s_name DO synerr("A name is missing")
    v!i := rname()
    i := i+1
  }
  a := newvec(i+1)
  h1!a, h2!a, h3!a := s_comma, i, b
  FOR j = 1 TO i-1 DO a!(j+2) := v!j
  RESULTIS a
}

AND rname() = VALOF
{ LET a = wordnode
  checkfor(s_name, "Name expected")
  RESULTIS a
}

AND rarg() = VALOF
{ LET a, ln = 0, lineno
//sawritef("rarg: token=%s*n", opstr(token))
  SWITCHON token INTO
  { DEFAULT:
      RESULTIS 0  // Not suitable as an unparenthesised argument

    CASE s_nil:
    CASE s_true:
    CASE s_false:
    CASE s_int:
    CASE s_real:
    CASE s_stringconst:
    CASE s_name:
    CASE s_lparen:
      RESULTIS rbexp()
  }
}
 
LET rbexp(n) = VALOF
{ LET a, op, ln = 0, token, lineno
// writef("rbexp: op=%s*n",opstr(op))
  SWITCHON op INTO
 
  { DEFAULT:
      synerr("Error in expression")

    CASE s_name:
      a := wordnode
      lex()
   
apply:
    { LET ln = lineno
      LET b = rarg()
      UNLESS b RESULTIS a
      a := mk3(s_apply, a, b, ln)
    } REPEAT
 
    CASE s_lparen:
      lex()
      TEST token=s_rparen
      THEN a := nilnode
      ELSE a := rcom(0)
      checkfor(s_rparen, "')' missing")
      IF n<=8 DO a := mk3(s_paren, a, ln)
      GOTO apply

    CASE s_true:
    CASE s_false:
    CASE s_nil:
    CASE s_stringconst:
    CASE s_sys:
    CASE s_dummy:
      a := wordnode
      lex()
      RESULTIS a
   
    CASE s_int:
      a := mk2(op, decval)
      lex()
      RESULTIS a
      GOTO apply
 
    CASE s_real:
      a := mk2(op, fltval)
      lex()
      GOTO apply
      RESULTIS a
 
    CASE s_noshare:
      UNLESS n<=36 DO synerr("'$' or 'sys' out of context")
      RESULTIS mk3(op, rnexp(38), ln)
 
    CASE s_plus:
      UNLESS n<=30 DO synerr("'+' out of context")
      RESULTIS rnexp(32)
 
    CASE s_minus:
      UNLESS n<=30 DO synerr("'-' out of context")
      a := rnexp(32)
      TEST h1!a=s_int | h1!a=s_real
      THEN h2!a := - h2!a
      ELSE a := mk2(s_neg, a)
      RESULTIS a
 
    CASE s_not:
      UNLESS n<=24 DO synerr("'not' out of context")
      RESULTIS mk2(s_not, rnexp(26))
  }
}
 
AND rnexp(n) = VALOF { lex(); RESULTIS rexp(n) }
 
AND rexp(n) = VALOF
{ LET a, b, p = rbexp(n), 0, 0

  { LET op, ln = token, lineno
//writef("rexp: op=%s*n", opstr(op))
    SWITCHON op INTO
    { DEFAULT:
        RESULTIS a
 

      CASE s_comma:
        IF n>14 RESULTIS a
        { LET i = 1
          LET v = VEC 500
          WHILE token=s_comma DO
          { v!i := rnexp(16)
            i := i+1
          }
          b := a
          a := newvec(i+1)
          a!0, a!1, a!2 := s_comma, i, b
          FOR j = 1 TO i-1 DO a!(j+2) := v!j
//sawritef("rexp: s_comma i=%n*n", i)
          LOOP
        }

      CASE s_aug:
        IF n>16 RESULTIS a
        a := mk4(s_aug, a, rnexp(18), ln)
        LOOP

      CASE s_cond:
        IF n>18 RESULTIS a
        b := rnexp(18)
        checkfor(s_pling, "Bad conditional expression")
        a := mk5(s_cond, a, b, rexp(18), ln)
        LOOP

      CASE s_logor:
        IF n>20 RESULTIS a
        a := mk4(op, a, rnexp(22), ln)
        LOOP

      CASE s_logand:
        IF n>22 RESULTIS a
        a := mk4(op, a, rnexp(24), ln)
        LOOP

      CASE s_eq:CASE s_le:CASE s_ls:CASE s_ne:CASE s_ge:CASE s_gr:
        IF n>26 RESULTIS a
        a := mk4(op, a, rnexp(30), ln)
        LOOP

      CASE s_plus:CASE s_minus:
        IF n>30 RESULTIS a
        a := mk4(op, a, rnexp(32), ln)
        LOOP

      CASE s_mult:CASE s_div:
        IF n>32 RESULTIS a
        a := mk4(op, a, rnexp(34), ln)
        LOOP

      CASE s_power:
        IF n>36 RESULTIS a
        a := mk4(op, a, rnexp(34), ln)
        LOOP

      CASE s_percent:
        IF n>36 RESULTIS a
        lex()
        UNLESS token=s_name DO synerr("Name expected in '%' construct")
        b := rname()
        a := mk4(s_comma, 2, a, rexp(38))
        a := mk4(s_apply, b, a, ln)
        LOOP
    }
  } REPEAT
}

AND rncom(n) = VALOF
{ lex()
  RESULTIS rcom(n)
}

AND rcom(n) = VALOF
{ LET a = rbcom(n)

  { LET op, ln = token, lineno
    SWITCHON op INTO
 
    { DEFAULT:
        BREAK
 
      CASE s_seq:
        IF n>6 RESULTIS a
        a := mk4(s_seq, a, rncom(6), ln)
        LOOP

      CASE s_where:
        IF n>2 RESULTIS a
        a := mk4(s_where, a, rnbdef(0), ln)
        LOOP

      CASE s_colon:
        UNLESS h1!a=s_name & n<=8 DO
          synerr("Syntax error in label")
        a := mk5(s_colon, a, rncom(8), 0, ln)
        LOOP
    }
  } REPEAT

  RESULTIS a
}

AND rbcom(n) = VALOF
{ LET op, ln, a, b = token, lineno, 0, 0

  SWITCHON op INTO
  { DEFAULT: // Must be an expression
    { a := rexp(n)
      ln := lineno
      IF token=s_ass RESULTIS mk4(s_ass, a, rnexp(14), ln)
      RESULTIS a
    }

    CASE s_let:
    { UNLESS n=0 DO synerr("'let' out of context")
    //writef("rcom(%n): entered*n", n)
      a := rndef(0)
      checkfor(s_in, "'in' expected in 'let' construct")
      RESULTIS mk4(s_let, a, rcom(0), ln)
    }

    CASE s_lambda:
    { LET v = VEC 50 // For the list of bound variables
      AND i = 0
      UNLESS n=0 DO synerr("'fn' out of context")
      lex()
      WHILE i<=50 DO
      { UNLESS token=s_lparen | token=s_name BREAK
        v!i := rbv()
        i := i+1
      }
      IF i=0 DO synerr("No bound variable list after 'fn'")
      checkfor(s_dot, "'.' missing in 'fn' construct")
      a := rcom(0)
      WHILE i>0 DO
      { i := i-1
        a := mk4(s_lambda, v!i, a, ln)
      }
      RESULTIS a
    }

    CASE s_valof:
      UNLESS n<=4 DO synerr("'valof' out of context")
      RESULTIS mk3(op, rncom(6), ln)
 
    CASE s_test:
      UNLESS n<=10 DO synerr("'test' out of context")
      a := rnexp(20)
      SWITCHON token INTO
      { DEFAULT:
          synerr("Bad 'test' command")

        CASE s_then:
          b := rncom(8)
          checkfor(s_ifnot, "'else' expected")
          RESULTIS mk5(s_cond, a, b, rncom(8), ln)

        CASE s_ifso:
          b := rncom(8)
          checkfor(s_ifnot, "'ifnot' expected")
          RESULTIS mk5(s_cond, a, b, rncom(8), ln)

        CASE s_ifnot:
          b := rncom(8)
          checkfor(s_ifso, "'ifnot' expected")
          RESULTIS mk5(s_cond, a, rncom(8), b, ln)
      }


    CASE s_while:
    { UNLESS n<=10 DO synerr("'while' out of context")     
      a := rnexp(20)
      checkfor(s_do, "'do' expected")
      RESULTIS mk5(s_while, a, rcom(8), ln)
    }

    CASE s_if:
    { UNLESS n<=10 DO synerr("'if' out of context")     
      a := rnexp(20)
      checkfor(s_do, "'do' expected")
      RESULTIS mk5(s_cond, a, rcom(8), dummynode, ln)
    }

    CASE s_goto:
      RESULTIS mk3(s_goto, rnexp(38), ln)

    CASE s_res:
      RESULTIS mk3(s_res, rnexp(14), ln)

    CASE s_dummy:
      RESULTIS dummynode

  }
}

LET opstr(op) = VALOF SWITCHON op INTO
{ DEFAULT:          writef("*nopstr: unknown op: %n*n", op)
abort(999)
                    RESULTIS "###Unknown op ###"

  CASE s_ass:         RESULTIS "Ass"
  CASE s_and:         RESULTIS "And"
  CASE s_apply:
  CASE i_apply:       RESULTIS "Apply"
  CASE s_aug:         RESULTIS "Aug"
  CASE s_bar:         RESULTIS "Bar"
  CASE t_basicfn:     RESULTIS "Basicfn"
  CASE i_blocklink:   RESULTIS "Blocklink"
  CASE t_closure:     RESULTIS "Closure"
  CASE s_colon:       RESULTIS "Colon"
  CASE s_comma:       RESULTIS "Comma"
  CASE s_cond:        RESULTIS "Cond"
  CASE i_decllabel:   RESULTIS "Decllabel"
  CASE i_declname:    RESULTIS "Declname"
  CASE i_declnames:   RESULTIS "Declnames"
  CASE s_def:         RESULTIS "Def"
  CASE s_div:         RESULTIS "Div"
  CASE s_do:          RESULTIS "Do"
  CASE s_dot:         RESULTIS "Dot"
  CASE s_dummy:
  CASE t_dummy:       RESULTIS "Dummy"
  CASE s_else:        RESULTIS "Else"
  CASE s_eof:         RESULTIS "Eof"
  CASE s_eq:          RESULTIS "Eq"
  CASE t_env:         RESULTIS "Env"
  CASE s_false:
  CASE i_false:
  CASE t_false:       RESULTIS "False"
  CASE i_finish:      RESULTIS "Finish"
  CASE i_formClosure: RESULTIS "FormClosure"
  CASE i_formLvalue:  RESULTIS "FormLvalue"
  CASE i_formRvalue:  RESULTIS "FormRvalue"
  CASE s_ge:          RESULTIS "Ge"
  CASE s_goto:        RESULTIS "Goto"
  CASE s_gr:          RESULTIS "Gt"
  CASE t_guess:       RESULTIS "Guess"
  CASE i_halt:        RESULTIS "Halt"
  CASE s_if:          RESULTIS "If"
  CASE s_ifso:        RESULTIS "Ifso"
  CASE s_ifnot:       RESULTIS "Ifnot"
  CASE s_in:          RESULTIS "In"
  CASE i_initname:    RESULTIS "Initname"
  CASE i_initnames:   RESULTIS "Initnames"
  CASE s_int:
  CASE t_int:         RESULTIS "Int"
  CASE s_jj:
  CASE i_jj:
  CASE t_jj:          RESULTIS "Jj"
  CASE i_jump:        RESULTIS "Jump"
  CASE i_jumpF:       RESULTIS "JumpF"
  CASE s_lab:         RESULTIS "Lab"
  CASE t_label:       RESULTIS "Label"
  CASE s_lambda:      RESULTIS "Lambda"
  CASE s_le:          RESULTIS "Le"
  CASE s_let:         RESULTIS "Let"
  CASE i_loadE:       RESULTIS "LoadE"
  CASE i_loadF:       RESULTIS "LoadF"
  CASE i_loadGuess:
  CASE t_loadGuess:   RESULTIS "LoadGuess"
  CASE i_loadJ:       RESULTIS "LoadJ"
  CASE i_loadL:       RESULTIS "LoadL"
  CASE i_loadN:       RESULTIS "LoadN"
  CASE i_loadR:       RESULTIS "LoadR"
  CASE i_loadS:       RESULTIS "LoadS"
  CASE s_logand:
  CASE i_logand:      RESULTIS "Logand"
  CASE i_logor:       RESULTIS "Logor"
  CASE i_lose1:       RESULTIS "Lose1"
  CASE s_lparen:      RESULTIS "Lparen"
  CASE s_ls:          RESULTIS "Lt"
  CASE s_lsect:       RESULTIS "Lsect"
  CASE t_lvalue:      RESULTIS "Lvalue"
  CASE i_members:     RESULTIS "Members"
  CASE s_minus:       RESULTIS "Minus"
  CASE s_mpt:         RESULTIS "Mpt"
  CASE s_mult:
  CASE i_mult:        RESULTIS "Mult"
  CASE s_name:        RESULTIS "Name"
  CASE s_ne:          RESULTIS "Ne"
  CASE s_neg:         RESULTIS "Neg"
  CASE s_nil:
  CASE t_nil:         RESULTIS "Nil"
  CASE t_nils:        RESULTIS "Nils"
  CASE i_norestart:   RESULTIS "Norestart"
  CASE s_noshare:     RESULTIS "Noshare"
  CASE s_not:
  CASE i_not:         RESULTIS "Not"
  CASE i_okrestart:   RESULTIS "Okrestart"
  CASE s_percent:     RESULTIS "Percent"       
  CASE s_paren:       RESULTIS "Paren"       
  CASE s_pling:       RESULTIS "Pling"
  CASE s_plus:
  CASE i_plus:        RESULTIS "Plus"
  CASE s_power:       RESULTIS "Power"
  CASE s_real:
  CASE t_real:        RESULTIS "Real"       
  CASE s_rec:         RESULTIS "Rec"       
  CASE s_res:         RESULTIS "Res"
  CASE i_reslink:     RESULTIS "Reslink"
  CASE i_restart:     RESULTIS "Restart"
  CASE i_restoreE1:   RESULTIS "RestoreE1"
  CASE i_return:      RESULTIS "Return"
  CASE s_rparen:      RESULTIS "Rparen" 
  CASE s_rsect:       RESULTIS "Rsect" 
  CASE i_rvrestart:   RESULTIS "Rvrestart"
  CASE i_save:        RESULTIS "Save"
  CASE s_sbra:        RESULTIS "Sbra"
  CASE s_seq:         RESULTIS "Seq"
  CASE i_setlabEs:    RESULTIS "SetlabEs"
  CASE i_setup:       RESULTIS "Setup"
  CASE s_sket:        RESULTIS "Sket"
  CASE s_stringconst: RESULTIS "Stringconst"
  CASE t_string:      RESULTIS "String"
  CASE t_stack:       RESULTIS "Stack"
  CASE s_sys:
  CASE i_sys:
  CASE t_sys:         RESULTIS "Sys"
  CASE s_test:        RESULTIS "Test"
  CASE i_testEmpty:   RESULTIS "TestEmpty"
  CASE s_then:        RESULTIS "Then"
  CASE s_true:
  CASE i_true:
  CASE t_true:        RESULTIS "True"
  CASE i_tuple:
  CASE t_tuple:       RESULTIS "Tuple"
  CASE i_update:      RESULTIS "Update"
  CASE s_valdef:      RESULTIS "Valdef"
  CASE s_valof:       RESULTIS "Valof"
  CASE s_where:       RESULTIS "Where"
  CASE s_while:       RESULTIS "While"
  CASE s_within:      RESULTIS "Within"
}

LET plist(x, n, d) BE
{ LET op, size, ln = ?, 0, 0
  LET v = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  writef("%i5: ", x)
  
  IF x=0 DO { writes("Null"); RETURN  }
 
  op := h1!x

  SWITCHON op INTO
  { DEFAULT:
      writef("Default op=%s*n", opstr(op)); RETURN

    CASE s_int:  // x -> [Int, value]
      writef("Int %i4", h2!x);          RETURN
	 
    CASE s_real: // x -> [Real, value]
      writef("Real %6.3f", h2!x);       RETURN

    CASE s_name: // x -> [Name, link, <chars>] link is initially the hash link
      writef("Name %s", @h3!x);         RETURN
	 
    CASE s_stringconst: // [ Stingconst, <packed characters> ] 
    { LET s = x+1
      writef("Stringconst *'")
      FOR i = 1 TO s%0 SWITCHON s%i INTO
      { DEFAULT:   wrch(s%i); ENDCASE
        CASE '*n': writes("**n"); ENDCASE
        CASE '*p': writes("**p"); ENDCASE
        CASE '*t': writes("**t"); ENDCASE
      }
      writef("*'")
      RETURN
    }

    CASE s_colon:
      size, ln := 3, h5!x; ENDCASE

    CASE s_cond: CASE s_test: CASE s_percent:
      size, ln := 4, h5!x; ENDCASE

    CASE s_power: CASE s_mult: CASE s_div: CASE s_plus: CASE s_minus:
    CASE s_eq: CASE s_ne: CASE s_ls: CASE s_gr: CASE s_le: CASE s_ge:
    CASE s_logand: CASE s_logor: CASE s_aug:
    CASE s_let: CASE s_where: CASE s_within:
    CASE s_lab:
    CASE s_ass: CASE s_apply: CASE s_lambda:
    CASE s_def: CASE s_valdef: CASE s_tuple: CASE s_seq:
    CASE s_if:
      size, ln := 3, h4!x; ENDCASE

    CASE s_comma: CASE s_and:
      // x -> [op, n, a1 ,..., an]
      size := h2!x+1
//sawritef("plist: Comma size=%n*n", size)
      x := x+1
      ENDCASE

    CASE s_noshare:
    CASE s_rec:
    CASE s_valof: 
    CASE s_goto: 
    CASE s_res:
    CASE s_sys:
    CASE s_paren:
    CASE s_not:
      size, ln := 2, h3!x
      ENDCASE

    CASE s_true: CASE s_false:
    CASE s_nil: CASE s_mpt:
    CASE s_dummy:
      size := 1;  //man #####
      ENDCASE
  }
 
  IF n=d DO { writes("Etc"); RETURN }
  writef("%s", opstr(op))
  IF op=s_and | op=s_comma DO writef(" %n", size-1)
  IF ln DO writef("  -- line %n", ln)
  FOR i = 2 TO size DO
  { newline()
    FOR j=0 TO n-1 DO writes( v!j )
    writes("**-")
    v!n := i=size->"  ","! "
    plist(h1!(x+i-1), n+1, d)
  }
}


AND trnerr(mess, a) BE
{ writes("Error")
  IF procname DO writef(" in %s", @h3!procname)
  IF comline DO writef(" near line %n", comline)
  writes(":   ")
  writef(mess, a)
  newline()
  errcount := errcount + 1
  IF errcount >= errmax DO fatalerr("*nCompilation aborted*n")
}

AND trprog(x) BE
{ LET n = ?
  FOR i = 0 TO labmax DO labv!i, refv!i := -1, 0

  comline, procname, labnumber := 1, 0, 0
  ssp, msp := 0, 1

  IF optCode DO writef("*nCompiled code:*n*n")

  codep := 1
  finishLoc    := codep; outf(i_finish)
  restartLoc   := codep; outf(i_restart)
  rvrestartLoc := codep; outf(i_rvrestart)
  okrestartLoc := codep; outf(i_okrestart)
  norestartLoc := codep; outf(i_norestart)
  startLoc     := codep
  
  n := nextlab()
  outfl(i_setup, n)

  translabels(x)
//writef("trprog: calling trans(%n, m_val)*n", x)
//abort(1001)
  trans(x, m_val)
  UNLESS ssp=1 DO writef("*nSSP error*n")
  outf(i_finish)
  //writef("*ntrprog: msp=%n*n", msp)
  //abort(1234)
  outlabset(n, msp)

  resolvelabels()
}

LET trans(x, mode) BE
// x       is the program
// mode is m_val or m_ref
{ LET op = h1!x

  IF x=0 DO
  { writes("*nExpression missing*n")
    outf(i_nil)
    upssp(1)
    IF mode=m_ref DO outf(i_formLvalue)
    RETURN
  }

//writef("trans: x=%n op=%s*n", x, opstr(op))
  SWITCHON op INTO
  { DEFAULT:
      writef("trans: DEFAULT case reached op=%s*n", opstr(op))
      abort(999)
      RETURN

    CASE s_let:          // x -> [Let, def, body, ln]
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      comline := h4!x
      transrhs(h2!x)
      outfl(i_blocklink, lab1)
      IF ssp=msp DO msp := ssp+1
      transscope(x, lab2, mode)
      outlab(lab1)             
      RETURN
    }
  
    CASE s_def:
    //writef("trans: CASE def:*n")
    //abort(1000)
      transrhs(h2!x)
      declnames(h2!x)
      translabels(h3!x)
      trans(h3!x, m_val)
      RETURN

    CASE s_mult: CASE s_div: CASE s_power: CASE s_plus: CASE s_minus:
    CASE s_eq: CASE s_ne: CASE s_ls: CASE s_le: CASE s_gr: CASE s_ge:
    CASE s_logand: CASE s_logor:
      trans(h3!x, m_val)
      trans(h2!x, m_val) 
      outf(cvs2i(op))
      ssp := ssp-1
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_aug:
      trans(h3!x, m_ref)
      trans(h2!x, m_val)
      outf(i_aug)
      ssp := ssp-1
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_apply:
      trans(h3!x, m_ref)  // The argument, often a tuple
      trans(h2!x, m_ref)  // A closure, tuple or sys
      outf(i_apply)
      ssp := ssp-1
      IF mode=m_val DO outf(i_formRvalue)
      RETURN

    CASE s_pos:
    CASE s_neg:
    CASE s_not:
      trans(h2!x, m_val)
      outf(cvs2i(op))
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_noshare:
      trans(h2!x, m_val)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_comma:
    { LET len = length(x)
      LET r(x) BE trans(x, m_ref)
      mapb(r, x)
      outfn(i_tuple, len)
      ssp := ssp - len + 1
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_lambda:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      LET lab3 = nextlab()
      outfl(i_formClosure, lab1)
      upssp(1)
      outfl(i_jump, lab2)
      outlab(lab1)
      transscope(x, lab3, m_ref)
      outlab(lab2)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_colon:
      IF h4!x=0 DO
      { trnerr("Label %s improperly used", h3!(h2!x))
      }
      outlab(h4!x)
      trans(h3!x, mode)
      RETURN

    CASE s_seq:
      trans(h2!x, m_val)
      outf(i_lose1)
      trans(h3!x, mode)
      RETURN

    CASE s_valof:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      outfl(i_reslink, lab1)
      ssp := ssp+1
      IF ssp>=msp DO msp := ssp+1
      { LET a, b = ssp, msp
        ssp, msp := 0, 1
        outfl(i_save, lab2)
        outf(i_jj)
        outf(i_formLvalue)
        outfn(i_declname, nameres)
        translabels(h2!x)
        trans(h2!x, m_ref)
        outf(i_return)
        UNLESS ssp=1 DO trnerr("SSP Error")
        outlabset(lab2, msp)
        ssp, msp := a, b
      }
      outlab(lab1)
      IF mode=m_val DO outf(i_formRvalue)
      RETURN
    }

    CASE s_res:
      trans(h2!x, m_ref)
      outf(i_res)
      ssp := ssp-1
      RETURN

    CASE s_goto:
      trans(h2!x, m_val)
      outf(i_goto)
      ssp := ssp-1
      RETURN

    CASE s_cond:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      trans(h2!x, m_val)
      outfl(i_jumpF, lab1)
      ssp := ssp-1
      trans(h3!x, mode)
      outfl(i_jump, lab2)
      outlab(lab1)
      ssp := ssp-1
      trans(h4!x, mode)
      outlab(lab2)
      RETURN
    }

    CASE s_while:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      outlab(lab2)
      trans(h2!x, m_val)
      outfl(i_jumpF, lab1)
      ssp := ssp-1
      trans(h3!x, m_val)
      outf(i_lose1)
      outfl(i_jump, lab2)
      outlab(lab1)
      outf(i_dummy)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_ass:
    { LET len = length(h2!x)
      comline := h4!x
      trans(h2!x, m_ref)
      trans(h3!x, m_val)
      outfn(i_update, len)
      ssp := ssp-1
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN
    }

    CASE s_paren:
      translabels(h2!x)
      trans(h2!x, mode)
      RETURN

    CASE s_nil:
    CASE s_dummy:
    CASE s_true:
    CASE s_false:
    CASE s_sys:
      outf(cvs2i(op))
      upssp(1)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_name:
      //writef("s_name: %s %x8 %x8*n", @h3!x, h3!x, h4!x)
      outfvar((mode=m_val -> i_loadR, i_loadL), str2sxvstr(@h3!x))
      upssp(1)
      RETURN

    CASE s_stringconst:
      //writef("s_stringconst: %s*n", @h2!x)
      outfvar(i_loadS, str2sxvstr(@h2!x))
      upssp(1)
      IF mode=m_ref DO outf(i_formLvalue)
      RETURN

    CASE s_int:
      outfn(i_loadN, h2!x)
      upssp(1)
      RETURN

    CASE s_real:
      outfflt(i_loadF, h2!x)
      upssp(1)
      RETURN
  }
}

AND cvs2i(op) = VALOF SWITCHON op INTO
{ DEFAULT:        trnerr("System error in cvs2i,op=%s*n", op)
                  RESULTIS 0
	    
  CASE s_true:    RESULTIS i_true
  CASE s_false:   RESULTIS i_false
  CASE s_mult:    RESULTIS i_mult
  CASE s_div:     RESULTIS i_div
  CASE s_power:   RESULTIS i_power
  CASE s_plus:    RESULTIS i_plus
  CASE s_minus:   RESULTIS i_minus
  CASE s_neg:     RESULTIS i_neg
  CASE s_eq:      RESULTIS i_eq
  CASE s_ne:      RESULTIS i_ne
  CASE s_ls:      RESULTIS i_ls
  CASE s_le:      RESULTIS i_le
  CASE s_gr:      RESULTIS i_gr
  CASE s_ge:      RESULTIS i_ge
  CASE s_not:     RESULTIS i_not
  CASE s_logand:  RESULTIS i_logand
  CASE s_logor:   RESULTIS i_logor
}

AND findlabels(x) = VALOF
{ IF x=0 RESULTIS 0
  SWITCHON h1!x INTO
  { DEFAULT:
      RESULTIS 0

    CASE s_colon:
    { LET lab = nextlab()
      h4!x := lab
      outfsl(i_decllabel, h2!x, lab)
      RESULTIS 1 + findlabels(h3!x)
    }

    CASE s_paren:
      RESULTIS findlabels(h2!x)

    CASE s_cond:
      RESULTIS findlabels(h3!x) +
               findlabels(h4!x)

    CASE s_while:
      RESULTIS findlabels(h3!x)

    CASE s_seq:
      RESULTIS findlabels(h2!x) +
               findlabels(h3!x)
  }
}

AND translabels(x) BE
{ LET n = findlabels(x)
  IF n DO outf(i_setlabEs, n)
}

AND transrhs(x) BE
{ IF x=0 RETURN

  SWITCHON h1!x INTO
  { DEFAULT:
      RETURN

    CASE s_and:
    { LET len = length(x)
      mapb(transrhs, x)
      outfn(i_tuple, len)
      ssp := ssp - len + 1
      outf(i_formLvalue)
      RETURN
    }

    CASE s_valdef:
      trans(h3!x, m_ref)
      RETURN

    CASE s_rec:
      outf(i_loadE)
      upssp(1)
      declguesses(h2!x)
      transrhs(h2!x)
      initnames(h2!x)
      loaddefinee(h2!x)
      outf(i_restoreE1)
      ssp := ssp-1
      RETURN

    CASE s_within:
    { LET lab1 = nextlab()
      LET lab2 = nextlab()
      transrhs(h2!x)
      outfl(i_blocklink, lab1)
      IF ssp=msp DO msp := ssp+1
      { LET a, b = ssp, msp
        ssp, msp := 0, 1
        outfl(i_save, lab2) // lab2 will be set to the
	                    // required size of the stack.
        declnames(h2!x)
        transrhs(h3!x)
        outf(i_return)
        UNLESS ssp=1 DO trnerr("SSP error")
        outlabset(lab2, msp)
        ssp, msp := a, b
      }
      outlab(lab1)
      RETURN
    }
  }
}

AND declnames(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad bound variable list")
      RETURN

    CASE s_name:
      outfname(i_declname, x)
      ssp := ssp-1
      RETURN

    CASE s_comma:
      outfn(i_declnames, length(x))
      ssp := ssp-1
      mapf(outnamepos, x)
      RETURN

    CASE s_and:
    { LET len = length(x)
      outfn(i_members, len)
      upssp(len-1)
      mapf(declnames, x)
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      declnames(h2!x)
      RETURN

    CASE s_within:
      declnames(h3!x)
      RETURN

    CASE s_mpt:
      outf(i_testEmpty)
      ssp := ssp-1
      RETURN
  }
}

AND loaddefinee(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outf(i_loadR); outnamepos(x)
      upssp(1)
      outf(i_formLvalue)
      RETURN

    CASE s_and:
    CASE s_comma:
    { LET len = length(x)
      mapb(loaddefinee, x)
      outfn(i_tuple, len)
      ssp := ssp - len + 1
      outf(i_formLvalue)
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      loaddefinee(h2!x)
      RETURN

    CASE s_within:
      loaddefinee(h3!x)
      RETURN

  }
}

AND declguesses(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outf(i_loadGuess)
      IF ssp=msp DO msp := ssp+1
      outf(i_declname); outnamepos(x)
      RETURN

    CASE s_and:
    CASE s_comma:
      mapf(declguesses, x)
      RETURN

    CASE s_rec:
    CASE s_valdef:
      declguesses(h2!x)
      RETURN

    CASE s_within:
      declguesses(h3!x)
      RETURN
  }
}

AND initnames(x) BE
{ IF x=0 RETURN
  SWITCHON h1!x INTO
  { DEFAULT:
      trnerr("Bad definition")
      RETURN

    CASE s_name:
      outf(i_initname); outnamepos(x)
      ssp := ssp-1
      RETURN

    CASE s_and:
    { LET len = length(x)
      outfn(i_members, len)
      upssp(len-1)
      outfn(i_initnames, len)
      mapf(initnames, x)
      RETURN
    }

    CASE s_comma:
    { LET len = length(x)
      outfn(i_initnames, len)
      mapf(outnamepos, x)
      ssp := ssp-1
      RETURN
    }

    CASE s_rec:
    CASE s_valdef:
      initnames(h2!x)
      RETURN

    CASE s_within:
      initnames(h3!x)
      RETURN
  }
}

AND transscope(x, n, mode) BE
{ LET a, b = ssp, msp
  ssp, msp := 1, 1
  outfn(i_save, n)
  declnames(h2!x)
  translabels(h3!x)
  trans(h3!x, mode)
  outf(i_return)
  UNLESS ssp=1 DO trnerr("SSP error")
  outlabset(n, msp)
  ssp, msp := a, b
}

AND mapf(fn, x) BE
{ // x -> [op, len, <elements>]
  LET len = h2!x
  FOR i = 1 TO len DO fn(x!(i+1))
}

AND mapb(fn, x) BE
{ // x -> [op, len, <elements>]
  LET len = h2!x
  FOR i = len TO 1 BY -1 DO fn(x!(i+1))
}

AND length(x) = h1!x=s_and | h1!x=s_comma -> h2!x, 1

AND upssp(x) BE
{ ssp := ssp+x
  IF ssp>msp DO msp := ssp
}

AND wrf(form, a, b, c, d) BE IF optCode DO writef(form, a, b, c, d)

AND outf(op) BE
{ wrf("%i5: %s*n", codep, opstr(op))
  putc(op)
}

AND outfflt(op, val) BE
{ wrf("%i5: %s %6.3f*n", codep, opstr(op), val)
  putc(op); putc(val)
}

AND outfname(op, name) BE
{ LET namestr = @h3!name
  LET namepos = str2sxvstr(namestr)
  wrf("%i5: %s %n   // %s*n", codep, opstr(op), namepos, namestr)
  putc(op)
  putc(namepos)
}

AND outfvar(op, var) BE
{ LET namestr = strv+var
  wrf("%i5: %s %n   // %s*n", codep, opstr(op), var, namestr)
  putc(op)
  putc(var)
}

AND outfstring(op, x) BE
{ LET str = @h3!x
  LET strpos = str2sxvstr(str)
  outfn(op, strpos)
  IF optCode DO writef("%s %n  // %s*n", opstr(op), strpos, str)
  putc(op)
  putc(strpos)
}

AND outfv(op, var) BE
{ wrf("%i5: %s %s*n", codep, opstr(op), var)
  putc(op); putc(var)
}

AND outn(a) BE
{ wrf("%i5: %n*n", codep, a)
  putc(a)
}

AND outfn(op, a) BE
{ wrf("%i5: %s %n*n", codep, opstr(op), a)
  putc(op); putc(a)
}

AND outfnn(op,a, b) BE
{ wrf("%i5: %s %n %n*n", codep, opstr(op), a, b)
  putc(op); putc(a); putc(b)
}

AND outfsl(op,a, b) BE
{ wrf("%i5: %s %s L%n*n", codep, opstr(op), a, b)
  putc(op); putc(a); putc(b)
}

AND outfl(op, lab) BE
{ wrf("%i5: %s L%n*n", codep, opstr(op), lab)
  putc(op); putref(lab)
}

AND outlab(lab) BE
{ wrf("%i5: Lab L%n*n", codep, lab)
  setlab(lab, codep)
}

AND outlabset(lab, val) BE
{ wrf("%i5: Equ L%n %n*n", codep, lab, val)
  setlab(lab, val)
}

AND outentry(l1, l2) BE
{ wrf("%i5: Entry L%n L%n*n", codep, l1, l2)
  putref(l2)
  setlab(l1, codep)
}

AND putc(w) BE TEST codep>codet
               THEN trnerr("More code space needed")
               ELSE { codev!codep := w
                      codep := codep+1
                    }

AND putref(lab) BE TEST codep>codet
                   THEN trnerr("More code space needed")
                   ELSE { codev!codep := refv!lab
                          refv!lab := codep
                          codep := codep+1
                        }

AND setlab(lab, addr) BE
{ ///writef("setlab: labv=%n lab=L%n addr=%n*n", labv, lab, addr)
  labv!lab := addr
}

AND nextlab() = VALOF
{ TEST labnumber>=labmax
  THEN fatalerr("More label space needed")
  ELSE labnumber := labnumber + 1
  RESULTIS labnumber
}
 

AND resolvelabels() BE FOR lab = 1 TO labnumber DO
{ LET p      = refv!lab
  AND labval = labv!lab
  //sawritef("resolvelabels: Entered*n")
  IF p & labval<0 TEST lab=1 THEN trnerr("start not defined")
                             ELSE trnerr("Label %n unset", lab)
  WHILE p DO { LET np = codev!p
               codev!p := labval
               p := np
             }
}

// End of the front end.

AND sxvpush(sxv, val) = VALOF
{ // Push a value into the self expanding vector sxv.
  LET upb = sxv!0      // =0 or the upb of v
  LET v   = sxv!1      // =0 or a getvec'd vector for the elements.
  LET p = v -> v!0, 0  // Position of the previous element, if any.
  // Initially upb, v, and p are all zero.
  // If v is not zero, v!0 will be the subscript of its latest element in v.
  // If the vector is full, pushval will allocate another larger vector
  // and copy the existing elements into it before pushing x.
  // The result is the subscript of v where val is stored.
  writef("sxvpush(%n,%i6): upb=%n v=%n p=%n*n", sxv,val,upb,v,p)
abort(7154)  
  IF p>=upb DO
  { LET newupb = 3*upb/2 + 100 // upb of the new larger vector
    LET newv = getvec(newupb)
    UNLESS newv DO
    { writef("More memory needed for pushval*n")
      abort(999)
      RETURN
    }
    sxv!0 := newupb // Update the control block
    sxv!1 := newv

    FOR i = 0 TO upb DO newv!i := v!i      // Copy the existing elements
    FOR i = upb+1 TO newupb DO newv!i := 0 // Pad with zeroes

    IF v DO freevec(v) // Free the old vector if it existed.

    v := newv
  }
  p := p+1
  v!0, v!p := p, val
  RESULTIS p
}


// The rest is xpal derived from the June 1970 version for
// the IBM 360 machine at MIT. The systematic changes are
// as follows.

// Most identifiers are changed to lower case.
// *( is replaced by !(
// Redundant parens are removed.
// The variables s, c and e are replaced by stack, pc and env.
// a and b are replaced by rega and regb.
// Runtime data is held in the self expanding vector palsxv.
// Garbage collection is done by copying accessible data in
// palsxv into a new self expanding vector and replacing the
// old one with the new copy.
// Floating point operations use the BCPL floating point
// operators providing 32 or 64 bit floating point depending
// on which BCPL system is being used.
// The required header declarations are combined with those
// at the start of pal.b.


LET execpal() = VALOF
{ // Execute one or more Pocode instructions.
  // It return 0 for successful completion
  // A non zero result indicates an error.
  
  // The PAL runtime memory is held the self expanding vector:
  // palsxv -> [datavupb, datav]
  // datav -> [pos,...] pos is the position of the next free element.
  
  // sp   abs pointer to the second to top item of the stck.
  // pc   abs pointer to the next instruction to execute in codev.
  //      This is only used after all the code has been compiled.
  // env  holds E -> [5, Env, link, name, value]

  // count is a counter that is incremented everyt ime a Pocode
  //       instruction is executed.

  // Garbage collecton is done by copying all reachable elements
  // of palsxv into a new self expanding vector. Then freeing the
  // old one and replacing palsxv with the new one.

  LET errorstr, lkpstr = "SYSTEMERROR", "LOOKUPNO"
  LET resstr = "#res#"
  
  // This implementation is designed to run on the modern
  // BCPL Cintcode system. It is based on the 1970 frontend
  // PAL/pal/docs/pal70-mabee-12jun70.pdf and the interpreter
  // PAL/pal/xpal70orig.b
  
  // For more efficient Cintcode
  // sp     is STACK+STACKP-2 of xpal70orig.b so
  // sp!1   is the top item on the stack and
  // sp!0   is the next item down
  // pc     is CODEV+C of xpal70orig.b so
  // pc!0   is the next Pocode op code
  //        Op codes are integers like i_loadR which
  //        calls function f_loadR when executed.

  retcode := 0 // The return code from the interpreter
               // 0 means halt was obeyed
 
  // Set the initial runtime state

  pc := codev + startLoc // Rel addr of the next instrucion to obey.
  oldc := 0

  sp   := 0         // Points to the second from top element, when set
  // sp!1           // Holds the top element of the stack
  // sp!0           // Holds the second from top element of the stack
  stack := 0
  env  := 0         // abd addr -> [5, Env, link, name, value] or zero
  rega := 0         // Zero or abs address od a runtime node.
  regb := 0         // Zero or abs address od a runtime node.

  // Setup the runtime data space
  datavupb := 0
  datav    := 0
  palsxv   := @datavupb
  sxvpush(palsxv, 0) // Ensure that datav is non zero

  gclimit := 1_000 // Choose a larger value later
  
  writef("XPAL 5 ENTERED*n*n")
  
//  writef("About to call mapliblist(f_libname)*n")
  //abort(1002)
  mapliblist(f_libname)
//abort(1008)

//  nameres := f_libname(resstr,   0)
//  //writef("nameres=%n*n", nameres)
//  f_libname(errorstr, 0)
//  f_libname(lkpstr,   0)

//abort(1008)

//abort(1003)
  gcmark := 0
  mapliblist(f_decllib)
  prstate("After mapliblist")

  // The following variables must be regarded as root by the
  // garbage collector.
  guessrv := list(2, t_guess)
  truerv  := list(2, t_true)
  falserv := list(2, t_false)
  nilrv   := list(2, t_tuple, 0)  // nil is a null tuple
  dummyrv := list(2, t_dummy)
  nilsrv  := list(2, t_nils)

//env := list(5, t_env, env, 0, env) // DUMMY ENV NODE USED BY DIAGNOSE
                                     // varname zero has value env

  rega    := list(3, t_basicfn, f_diagnose)
  errorlv := list(3, t_lvalue, rega)
  //writef("execpal: errorstr=%s corresponding var=%n*n",
  //        errorstr, str2sxvstr(errorstr))
  env     := list(5, t_env, env, str2sxvstr(errorstr), errorlv)
  rega := 0
prstate("After declaring errorstr")
  lookupnovar := str2sxvstr(lkpstr)
  lookupno := list(3, t_int, 0)
  rega := list(3, t_lvalue, lookupno)
  env := list(5, t_env, env, lookupnovar, rega)
  rega := 0

  errflag, floterr := FALSE, FALSE
  listv := listp
  restartc := 0
  nset := FALSE
  count := 0
  errct := 0

  done := FALSE

//writef("execpal: entering the execution loop*n*n")

  UNTIL done DO
  {
    IF optTrace DO
    { //prstate("About to execute an instruction")
      prstate("execpal")
      //abort(6464)
    }

    count := count+1

    SWITCHON h1!pc INTO
    { DEFAULT:
        writef("relpc=%i5: ILLEGAL Pocode op: %n*n", pc-codev, h1!pc)
        done := TRUE
        LOOP

      CASE i_name:        f_name();        LOOP
      CASE i_stringconst: f_stringconst(); LOOP
				
      CASE i_number:      f_number();      LOOP
    
      CASE i_setlabEs:    f_setlabEs();    LOOP
      CASE i_restoreE1:   f_restoreE1();   LOOP
      CASE i_formRvalue:  f_formRvalue();  LOOP
      CASE i_formLvalue:  f_formLvalue();  LOOP
      CASE i_tuple:       f_tuple();       LOOP
      CASE i_members:     f_members();     LOOP
      CASE i_loadGuess:   f_loadGuess();   LOOP
      CASE i_true:        f_true();        LOOP
      CASE i_false:       f_false();       LOOP
      CASE i_finish:      f_finish();      LOOP
      CASE i_lose1:       f_lose1();       LOOP
      CASE i_mult:        f_mult();        LOOP
      CASE i_div:         f_div();         LOOP
      CASE i_plus:        f_plus();        LOOP
      CASE i_minus:       f_minus();       LOOP
      CASE i_pos:         f_pos();         LOOP
      CASE i_neg:         f_neg();         LOOP
      CASE i_eq:          f_eq();          LOOP
      CASE i_ls:          f_ls();          LOOP
      CASE i_gr:          f_gr();          LOOP
      CASE i_le:          f_le();          LOOP
      CASE i_ne:          f_ne();          LOOP
      CASE i_ge:          f_ge();          LOOP
      CASE i_logand:      f_logand();      LOOP
      CASE i_logor:       f_logor();       LOOP
      CASE i_save:        f_save();        LOOP
      CASE i_apply:       f_apply();       LOOP
      CASE i_not:         f_not();         LOOP
      CASE i_jj:          f_loadJ();       LOOP
      CASE i_update:      f_update();      LOOP
      CASE i_res:         f_result();      LOOP
      CASE i_goto:        f_goto();        LOOP
      CASE i_loadR:       f_loadR();       LOOP
      CASE i_loadL:       f_loadL();       LOOP
      CASE i_loadS:       f_loadS();       LOOP
      CASE i_loadN:       f_loadN();       LOOP
      CASE i_loadF:       f_loadF();       LOOP
      CASE i_testEmpty:   f_testEmpty();   LOOP
      CASE i_declname:    f_declname();    LOOP
      CASE i_declnames:   f_declnames();   LOOP
      CASE i_initname:    f_initname();    LOOP
      CASE i_initnames:   f_initnames();   LOOP
      CASE i_formClosure: f_formClosure(); LOOP
      CASE i_jumpF:       f_jumpF();       LOOP
      CASE i_jump:        f_jump();        LOOP
      CASE i_decllabel:   f_decllabel();   LOOP
      CASE i_return:      f_return();      LOOP
      CASE i_blocklink:   f_blocklink();   LOOP
      CASE i_reslink:     f_reslink();     LOOP
      CASE i_power:       f_power();       LOOP
      CASE i_nil:         f_nil();         LOOP
      CASE i_sys:         f_sys();         LOOP
      CASE i_halt:        f_halt();        LOOP
      CASE i_dummy:       f_dummy();       LOOP
      CASE i_aug:         f_aug();         LOOP
      CASE i_setup:       f_setup();       LOOP
      CASE i_rvrestart:   f_rvrestart();   LOOP
    }
  }

  RESULTIS retcode
}

AND prstate(str) BE //IF FALSE DO
{ LET e = env
  LET s = datav+stack
  writef("State: datav=%n strv=%n s=%n sp=%n %s*n",
           datav, strv, s, sp, str)
  //writef("datav=%i5 sp=%n pc=%n+%n oldc=%n*n",
  //        datav, sp, codev, pc-codev, oldc)

  writef("rega=%n", rega)
  IF rega DO
  { writef(" ->*n")
    prvalue(rega, 5)
  }
  newline()
  writef("regb=%n", regb)
  IF regb DO
  { writef(" -> *n")
    prvalue(regb, 5)
  }
  newline()

  writef("stack=%n",stack)
  IF stack DO
  { LET s = datav+stack
    LET p = sp-stack-datav
    writef(" ->*n[")
    FOR i = 0 TO s!0 TEST i=1
    THEN writef(" %s", opstr(s!i))
    ELSE writef(" %n", s!i)
    writef(" ]*n")
    writef("  p=%n*n", p)
    IF p>4 DO { writef("  Top: at %n ", p+1); prvalue(sp!1); newline() }
    IF p>5 DO { writef("  2nd: at %n ", p);   prvalue(sp!0); newline() }
  }
  UNLESS stack DO newline()

  writef("env=%n:*n", env)
  IF e DO
  { FOR i = 1 TO 8 DO
    { // e -> [ 5, Env, link, name, val]
      LET p = datav+e
      LET link = h3!p
      writef("%i5: [%i5, %s, %i5, %n=%s, ",
              e, h1!p, opstr(h2!p), link, h4!p, strv+h4!p)
      prvalue(h5!p, 3)
      writef("]*n")
      UNLESS link BREAK
      e := link
      //abort(1001)
    }
  }

  writef("Next instruction:*n")
  writef("%i5: %s", pc-codev, opstr(h1!pc))

  SWITCHON h1!pc INTO
  { DEFAULT:
      FOR i = 1 TO Operands(h1!pc) DO writef(" %i2", pc!i)
      newline()
      ENDCASE
       
    CASE i_loadF:
      writef(" %5.3f*n", pc!1)
      ENDCASE
      
    CASE i_name:
    CASE i_loadL:
    CASE i_loadS:
      writef(" %n  // '%s'*n", pc!1, strv+pc!1)
      ENDCASE
  }
  abort(1111)
}

AND node(n) = VALOF
{ LET res = h1!datav + 1 // Rel address of the new node in datav
  LET args = @n
  FOR i = 0 TO n-1 DO sxvpush(palsxv, 0)
  RESULTIS res // relative to datav which may have been changed by sxvpush
}


AND list(n, a, b, c, d, e, f, g) = VALOF
{ LET res = sxvpush(palsxv, n) // Address rel to datav of next free location
                               // Note that datav may have been changed
  LET args = @n
//  writef("list n=%n a=%n res=%n datav=%n*n", n, a, res, datav)
  FOR i = 1 TO n-1 DO sxvpush(palsxv, args!i) // push values into datav

  IF FALSE DO
  { LET r = datav+res
    writef("list: => %n+%n -> [", datav, res)
    FOR i = 0 TO n-1 SWITCHON i INTO
    { DEFAULT:
        writef(" %n", r!i)
        ENDCASE

      CASE 1:
        //writef(" i=%n r=%n r!i=%n*n", i, r, r!i)
        writef(" %s", opstr(r!i))
	//writef("*nXXX*n")
        ENDCASE
      
      CASE 3:
      { LET val = r!i
        //writef(" i=%n val=%n*n", i, val)
        TEST a=t_env
        THEN writef(" %s", strv+val)
        ELSE writef(" %n", val)
     
        ENDCASE
      }
    }
    writef("]*n")
    IF a=t_env & c=0 DO abort(5678)
  }
  
  RESULTIS res // Address rel of item i palsxv
}

AND printf(mem, form, p) BE  // Not used ?????
{ LET fmt = form+mem
  LET i = 0

  { LET k = fmt%i
    i := i+1
    IF k=0 RETURN
    IF k='%' DO
    { LET n = 0;
      { k := fmt%i
        i := i+1
        UNLESS '0'<=k<='9' BREAK
        n := 10*n + k - '0'
      } REPEAT
      SWITCHON k INTO
      { DEFAULT:  wrch(k); LOOP
        CASE 'd': writed  (!p,     n); p := p+1; LOOP
        CASE 's': wrs     (mem+!p, n); p := p+1; LOOP
        CASE 'x': writehex(!p,     n); p := p+1; LOOP
      }
    }
    wrch(k)
  } REPEAT
}

AND wrs(s, n) BE // Only used by printf ?????
{ LET len = 0
  WHILE s%len DO len := len+1
  FOR i = len+1 TO n DO wrch(' ')
  FOR i = 0 TO len-1 DO wrch(s%i)
}

AND Operands1(op) = VALOF
{ LET n = Operands1(op)
  writef("Operands: op=%n=%s*n", op, opstr(op))
  writef("Operands => %n*n", n)
  abort(1000)
  RESULTIS n
}

AND Operands(op) = VALOF SWITCHON op INTO
{ DEFAULT:
    writef("*nOperands: op=%n=%s result unknown*n", op, opstr(op))
    abort(999)
    RESULTIS 0

  CASE i_name:
  CASE i_stringconst:
  CASE i_number:
  CASE i_save:
  CASE i_loadR:
  CASE i_loadL:
  CASE i_loadS:
  CASE i_loadN:
  CASE i_loadE:
  CASE i_loadF:
  CASE i_setup:
  CASE i_tuple:
  CASE i_blocklink:
  CASE i_declname:
    RESULTIS 1

  CASE i_loadGuess:
  CASE i_true:
  CASE i_false:
  CASE i_lose1:
  CASE i_mult:
  CASE i_div:
  CASE i_plus:
  CASE i_minus:
  CASE i_pos:
  CASE i_neg:
  CASE i_eq:
  CASE i_ls:
  CASE i_gr:
  CASE i_le:
  CASE i_ne:
  CASE i_ge:
  CASE i_logand:
  CASE i_logor:
  CASE i_not:
  CASE i_jj:
  CASE i_goto:
  CASE i_formRvalue:
  CASE i_formLvalue:
  CASE i_halt:
  CASE i_apply:
  CASE i_finish:
  CASE i_rvrestart:
  CASE i_return:
    RESULTIS 0
/*	
  CASE i_setlabEs:
  CASE i_restoreE1:
  CASE i_tuple:
  CASE i_members:
  CASE i_apply:
  CASE i_update:
  CASE i_res:
  CASE i_testEmpty:
  CASE i_declname:
  CASE i_declnames:
  CASE i_initname:
  CASE i_initnames:
  CASE i_formClosure:
  CASE i_jumpF:
  CASE i_jump:
  CASE i_decllabel:
  CASE i_return:
  CASE i_blocklink:
  CASE i_reslink:
  CASE i_power:
  CASE i_nil:
  CASE i_sys:
  CASE i_dummy:
  CASE i_aug:
    writef("Operands: op=%n=%s =>5*n", op, opstr(op))
    abort(999)
    RESULTIS 5
*/
}


// The rest is XPal dating from about June 1970



// XPALHD LAST MODIFIED ON FRIDAY, 12 JUNE 1970
// AT 5:29:07.24 BY R MABEE
// LISTING OF PAL RUN TIME SYSTEM (XPAL) HEADFILE AND BCPL/360 BASIC
// HEADFILE GOTTEN WITHIN SUPPRESSED BY NOLIST DIRECTIVE. TO OVERRIDE
// DIRECTIVE, SPECIFY ALLSOURCE OPTION TO BCPL COMPILER.
//>>> nolist
//>>> eject
//
//	 ******************************
//	 *                            *
//	 *           XPALHD           *
//	 *                            *
//	 *  (COMPATIBLE WITH PALSYS)  *
//	 *                            *
//	 ******************************
//
// GET BASIC BCPL/360 HEAD FILE
//>>> GET "BASIC"

/*
GET "libhdr"

GLOBAL 	// FLOTLIB GLOBALS
{	fadd	: 71 // FADD(REAL1,REAL2) = REAL1 + REAL2
	fsub	: 72 // FSUB(REAL1,REAL2) = REAL1 - REAL2
	fmult	: 73 // FMULT(REAL1,REAL2) = REAL1 * REAL2
	fdiv	: 74 // FDIV(REAL1,REAL2) = REAL1 / REAL2
	fpower	: 75 // FPOWER(REAL,INTEGER) = REAL ** INTEGER
	fumin	: 76 // FUMIN(REAL) = - REAL
	fabs	: 77 // FABS(REAL) = ABS REAL
	fgr	: 78 // FGR(REAL1,REAL2) = BOOLEAN
	fge	: 79 // FGE(REAL1,REAL2) = BOOLEAN
	feq	: 80 // FEQ(REAL1,REAL2) = BOOLEAN
	fne	: 81 // FNE(REAL1,REAL2) = BOOLEAN
	fls	: 82 // FLS(REAL1,REAL2) = BOOLEAN
	fle	: 83 // FLE(REAL1,REAL2) = BOOLEAN
	itor	: 84 // ITOR(INTEGER) = REAL
	rtoi	: 85 // RTOI(REAL) = INTEGER
	stof	: 86 // STOF(STRING) = REAL
	ftos	: 87 // FTOS(REAL,STRING) = STRING
	stoi	: 88 // STOI(STRING) = INTEGER
	writef	: 89 // WRITEF(REAL) WRITES REAL NUMBER
	floterr	: 90 // TRUE IF FL PT ERROR OCCURS
}


MANIFEST	// VECTOR APPLICATION
{	h1=0; h2=1; h3=2; h4=3; h5=4; h6=5; h7=6 }


MANIFEST	// AE NODES AND POCODE SYMBOLS
{	m_goto=148; m_res=149
	m_not=151; m_nil=152; stringconst=153; name=154
	m_plus=157; m_minus=158
	m_aug=160; m_logor=161; m_logand=162
	m_ge=163; m_ne=164; m_le=165; m_gr=166; m_ls=167; m_eq=168
	m_mult=169; m_div=170; m_power=171
	m_pos=173; m_neg=174; m_apply=175 }


MANIFEST	// POCODE SYMBOLS
{	m_loadl=181; m_loadr=182; m_loade=183; m_loads=184; m_loadn=185
	m_restoree1=187; m_loadguess=188
	m_formclosure=189; m_formlvalue=190; m_formrvalue=191
	m_members=192
	m_jump=195; m_jumpf=196; m_save=197; m_return=198
	m_testEmpty=199; m_lose1=200; m_update=201
	m_declname=203; m_declnames=204; m_initname=205; m_initnames=206
	m_decllabel=207; m_setlabes=208; m_blocklink=209; m_reslink=210
	m_setup=211
	integer=213; lab=214; param=215; equ=216 }


MANIFEST	// RUN-TIME NODE TYPES
{	m_dummy=220; jj=221; m_true=222; m_false=223; number=224
	m_tuple=225; closure=226; basicfn=227
	lvalue=228; string=229; nils=230; real=231
	label=232; guess=233; env=234; stack=235 }


GLOBAL	// PLACEMENT SET BY PALSYS
{	xpal:202; timeovfl:199; time_exceeded:93 }


GLOBAL	// RUN TIME SYSTEM GLOBAL FUNCTIONS
{	load:375; setparams:376; mapliblist:377; libname:378; decllib:379;
	loadl:38o; loadr:381; loadj:382; loade:383; loads:384; loadn:385;
	restoree1:386; r_true:387; r_false:388; loadguess:389; nil:390;
	dummy:391; formclosure:392; formlvalue:393; nextlv11:394; next11:395;
	formrvalue:396; tuple:397; members:398; r_not:399; r_logand:400;
	r_logor:401; aug:402; result:403; mult:404; div:405; plus:406;
	minus:407; power:408; pos:409; neg:410; r_eq:411; r_ne:412; r_ls:413;
	r_le:414; r_gr:415; r_ge:416; jump:417; jumpf:418; edbg:419;
	errdbg:420; errlvdbg:421; errokdbg:422; comdbg:423; okrestart:424;
	rvrestart:425; norestart:426; apply:427; save:428; r_return:429;
	testEmpty:430; lose1:431; r_goto:432; update:433; error:434;
	error1:435; printb:436; printa:437; equal:438; testnumbs2:439;
	testbools2:440; lvofname:441; nameoflv:442; restart:443;
	terminate:444; terminate1:445; lastfn1:446; writenode:447; node:448;
	nextarea:449; marklist:450; mark:451; list:452; split1:453;
	split2:454; declname:455; declnames:456; initname:457; initnames:458;
	r_name:459; nameerror:460; decllabel:461; setlabes:462;
	blocklink:463; reslink:464; setup:465; r_finish:466; print:467;
	userpage:468; stem:469; stern:470; conc:471; atom:472; null:473;
	length:474; istruthvalue:475; isnumber:476; isstring:477;
	isfunction:478; isenviroment:479; islabel:480; istuple:481;
	isreal:482; isdummy:483; share:484; ston:485; cton:486; ntoc:487;
	ntor:488; rton:489; rdchar:490; r_table:491; diagnose:492;
	lastfn:493; lookupine:494; saveenv:495 }


GLOBAL	// RUN TIME SYSTEM GLOBAL VARIABLES
{	a:501; b:502; c:503; codep:504; count:505; dummyrv:506; errct:507;
	errflag:508; errorlv:509; e:510; f:511; falserv:512; gcmark:513;
	guessrv:514; linep:515; linet:516; linev:517; listl:518; listp:519;
	listt:520; listv:521; lookupno:522; namechain:523; nameres:524;
	nilrv:525; nilsrv:526; nset:527; oldc:528; parv:529; q:530;
	refp:531; reft:532; refv:533; restartc:534; s:535; stackp:536;
	strb:537; strp:538; tlength:539; top:540; truerv:541 }


GLOBAL	// VARIABLES COMMON WITH PALSYS
{	ch		: 218 // LAST CHARACTER READ
	codefile	: 219 // POINTER TO POCODE STORAGE AREA
	codefilep	: 220 // POINTER TO NEXT WORD POCODE STORAGE
	dataflag	: 221 // INDICATES IF DATA FOLLOWS RUN CARD
	gcdbg		: 232 // INDICATES IF COLLECTER DEBUGGING ON
	input		: 234 // PRESENT INPUT STREAM
	lvch		: 251 // LVALUE OF CH
	maxct		: 252 // XPAL MAXIMUM CYCLE COUNT
	maxerr		: 253 // XPAL MAXIMUM ERROR COUNT
	stackwarning	: 269 // APPROXIMATE END BCPL RUN TIME STACK
	storage		: 272 // POINTER TO USABLE FREE STORAGE
	storaget	: 273 // POINTER TO END OF USABLE FREE STORAGE
	tupledepth	: 287 // XPAL MAXIMUM TUPLE PRINT DEPTH
	xpend		: 289 // GLOBAL LABEL
	xpendlevel	: 290 // LEVEL OF GLOBAL LABEL XPEND

// Extra globals
        output          : 291 // used in control calls
	control         : 292
	writechar       : 293
}

// Extra Manifests
MANIFEST {
	bytemax         = 255
}

*/

/*
LET xpal1() BE
{	LET errorstr, lkpstr = "SYSTEMERROR", "LOOKUPNO"
	LET v1 = VEC 1000
	LET v2 = VEC 2002
	LET v3 = VEC 150
 	//control(output, 3)
	writef("Calling mapliblist(f_libname)*n")
	writef("XXPAL 5 ENTERED*n*n")
	strp, strb := storaget, codefilep
	codep := storage
	namechain := 0
	nameres := "#RES#" // Originally "**RES**"
	abort(1000)

	mapliblist(f_libname)

	f_libname(nameres, 0)
	f_libname(errorstr, 0)
	f_libname(lkpstr, 0)
	RV codefilep := endstreamch
	parv := v1
	refv, reft := v2, 2000
	load ()
	codep!0 := i_finish
	listv, listp := codep+1, codep+1
	listt, listl := strp-1, strp-1
	linev, linep, linet := v3, v3+80, v3+79
	env, pc, rega, regb := 0, storage, 0, 0
	gcmark := 0

	mapliblist(f_decllib)

	guessrv := list(2,t_guess)
	truerv := list(2, t_true)
	falserv := list(2, t_false)
	nilrv := list(2, t_tuple, 0)
	dummyrv := list(2, t_dummy)
	nilsrv := list(2, t_nils)
	abort(5432)

	env := 0 //list(5,t_env,env,0,env) // DUMMY ENV NODE USED BY DIAGNOSE

	rega := list(3, t_basicfn, f_diagnose)
	errorlv := list(3, t_lvalue, rega)
	env := list(5, t_env, env, errorstr, errorlv)
	lookupno := list(3, t_int, 0)
	rega := list(3, t_lvalue, lookupno)
	env := list(5, t_env, env, lkpstr, rega)
	errflag, floterr := FALSE, FALSE
	listv := listp
	restartc := 0
	nset := FALSE
	count := 0
	errct := 0
	{	(pc!0)()
		count := count+1
		IF time_exceeded DO timeovfl()
	} REPEATWHILE count<maxct
	writes("*n*nEXECUTION CYCLE LIMIT EXCEEDED*n")
	terminate()
}
*/



//>>> eject
// XPAL1B
LET mapliblist(f) BE
{ f("PRINT",         f_print)
//  f("PAGE",          f_userpage)
//  f("STEM",          f_stem)
//  f("STERN",         f_stern)
//  f("CONC",          f_conc)
//  f("ATOM",          f_atom)
//  f("NULL",          f_null)
//  f("ORDER",         f_length)
//  f("ISTRUTHVALUE",  f_istruthvalue)
//  f("ISINTEGER",     f_isnumber)
//  f("ISREAL",        f_isreal)
//  f("ISSTRING",      f_isstring)
//  f("ISFUNCTION",    f_isfunction)
//  f("ISLABEL",       f_islabel)
//  f("ISTUPLE",       f_istuple)
//  f("ISDUMMY",       f_isdummy)
//  f("ISENVIRONMENT", f_isenvironment)
//  f("FINISH",        f_finish)
//  f("SHARE",         f_share)
//  f("STOI",          f_ston)
//  f("CTOI",          f_cton)
//  f("ITOC",          f_itoc)
//  f("RTOI",          f_rton)
//  f("ITOR",          f_ntor)
//  f("READCH",        f_rdchar)
//  f("DIAGNOSE",      f_diagnose)
//  f("LASTFN",        f_lastfn)
//  f("TABLE",         f_table)
//  f("LOOKUPINE",     f_lookupinE)
//  f("SAVEENV",       f_saveenv)
}

AND f_libname(x, y) = VALOF
{ // Store a name in the table of names
  // x is the BCPL string of the variable name
  // y is its value, ignored by libname.
  // The string x is looked up/stored in sxvstr
  // The result is the position in strv of the stored string.
  LET varname = str2sxvstr(x) // Put the name string in sxvstr
//writef("libname: x=%s y=%n varname=%n*n", x, y, varname)
//abort(1006)
  RESULTIS varname
}

AND f_decllib(x, y) BE
{ // x is the name of a builtin library function
  //   as a BCPL string.
  // y is the corresponding basicfn number.
  // A suitable item is added to the start of env.
  LET varname = str2sxvstr(x)
  //writef("f_decllib: x=%s %n*n", x, y, varname)
  rega := list(3, t_basicfn, y)
  rega := list(3, t_lvalue, rega)
  env  := list(5, t_env, env, varname, rega)
  rega := 0
  //writef("f_decllib: %n+%n -> [", datav, env)
  //FOR i = 0 TO 4 DO writef(" %n", datav!(env+i))
  //writef(" ]*n")
//  abort(1007)
}

// XPAL2

LET f_loadL() BE
{ LET var = pc!1
  LET str = strv+var

  //writef("f_loadL: pc!1=%n=%s*n", var, str)
  pc := pc+1
  rega := lvofname(var, env)
  TEST rega=nilrv
  THEN { rega := list(3, t_lvalue, rega)
	 errokdbg()
       }
  ELSE { push(rega); pc:=pc+1 }
}

AND f_loadR() BE  // [ LoadR, var ]
{ rega := lvofname(pc!1, env)
  TEST rega=nilrv
  THEN errdbg()
  ELSE { LET a = datav + rega
         rega := h3!a
	 push(rega)
	 pc := pc+2
       }
}

AND f_loadJ() BE
{ rega := list(5, t_jj, h4!stack, h5!stack, h6!stack )
  push(rega); pc:=pc+1
}

AND f_loadE() BE
{ rega := env
  push(rega); pc:=pc+1
}

AND f_loadS() BE
{ LET str = strv+pc!1
  rega := nilsrv
  FOR i = 1 TO str%0 DO
    rega := list(4, t_string, rega, str%i)
  pc := pc+1
  push(rega); pc:=pc+1
}

AND f_loadN() BE // Now only loads an int
{ writef("loadN: op loadN at pc=%n value=%n*n", pc, pc!1)
abort(9523)
  rega := list(3, t_int, pc!1 ) // Should be t_int,pc!1 not pc!1,pc!2
  pc := pc+1
  push(rega); pc:=pc+1
  prstate("At end of loadN")
}

AND f_loadF() BE // Not in xpal7, This loads a real
{ rega := list(3, t_real, pc!1)
  pc := pc+1
  push(rega); pc:=pc+1
}

AND f_restoreE1() BE
{ env := sp!0  //stack!(stackp-2)
  sp!0 := sp!1 // stack!(stackp-1) := stack!(stackp) after dec stackp
  sp   := sp-1
  pc := pc+1
}

AND f_true() BE
{ rega := truerv
  push(rega)
  pc:=pc+1
}

AND f_false() BE
{ rega := falserv
  push(rega); pc:=pc+1
}

AND f_loadGuess() BE
{ rega := mklvnode(guessrv)
  push(rega)
  pc:=pc+1
}

AND f_nil() BE
{ rega := mklvnode(nilrv)
  push(rega)
  pc:=pc+1
}

AND f_dummy() BE
{ rega := mklvnode(dummyrv)
  push(rega)
  pc:=pc+1
}

AND f_formClosure() BE
{ rega := list(4, t_closure, env, pc!1)
  push(rega)
  pc := pc+2
}

AND f_formLvalue() BE
{ rega := list(3, t_lvalue, sp!1)
  sp!1 := rega
  pc := pc+1
}

AND mklvnode(x) = VALOF
{ abort(1777)
  RESULTIS mklvnode1(x)
}
AND mklvnode1(x) = VALOF
{
  LET a = list(3, t_lvalue, x)
  writef("mklvnode: a=%n*n", a)
  RESULTIS a
}

AND nextlv11() BE
{ rega := mklvnode(rega)
  push(rega)
  pc:=pc+1
}

AND pushlva() BE
{ rega := mklvnode(rega)
  push(rega)
}

AND f_formRvalue() BE
{ UNLESS h2!(datav+sp!1) DO
  { writef("System error in f_formRvalue*n")
    abort(999)
  }
  sp!1 := h3!(datav+sp!1)
  pc := pc+1
}

AND f_tuple() BE
{ // Create a pointer to [n+2, Tuple, n, a1,a2,...,an]
  // where n is the argument of i_tuple
  LET n = pc!1 // Size of the tuple
  LET a = 0
  rega := node(n+3)
  a := datav+rega
  a!0, a!1, a!2 := n+3, t_tuple, n
  FOR i = 3 TO n+2 DO
  { a!i := sp!1
    sp := sp - 1
  }
  push(rega)
  pc:=pc+2
}

AND f_members() BE
{ LET n = pc!1
  LET a, b = 0, 0
  rega := pop()
  a := datav+rega
  b := datav+regb
  regb := h3!a
  FOR i = -2 TO n-3 DO
  { sp := sp+1
    sp!1 := b!(n-i)
  }
  pc := pc+2
}

AND f_not() BE
{ LET a = 0
  rega := pop()
  a := datav+rega

  IF h2!a=t_false DO
  { rega := truerv
    push(rega); pc:=pc+1
    RETURN
  }

  IF h2!a=t_true DO
  { rega := falserv
    push(rega)
    pc:=pc+1
  }
  
  error1("NOT", rega, 0)
  errdbg()
}

AND f_logand() BE
{ LET a = 0
  regb := pop(); rega := pop()
  a := datav+rega
  TEST testbools2()
  THEN { rega := h2!a=t_true -> regb, falserv
	 push(rega); pc:=pc+1
       }
  ELSE { error1("&", rega, regb)
	 rega := falserv
         errdbg()
       }
}

AND f_logor() BE
{ LET a = 0
  regb := pop(); rega := pop()
  a := datav+rega
  TEST testbools2()
  THEN { rega := h2!a=t_false -> regb, truerv
	 push(rega); pc:=pc+1
       }
  ELSE { error1("OR", rega, regb)
	 rega := falserv
	 errdbg()
       }
}

AND f_aug() BE
{ LET a= 0
  regb := pop(); rega := pop()
  a := datav+rega
  UNLESS h2!a=t_tuple DO
  { error1("AUG", rega, regb)
    rega := nilrv
    errdbg()
    RETURN
  }
  { LET n = h3!a
    LET tup = node(n+4)
    LET t = datav+tup
    t!0, t!1, t!2, t!(n+3) := n+4, t_tuple, n+1, regb
    FOR i = 3 TO n+2 DO t!i := a!i
    rega := tup
    push(rega); pc:=pc+1
  }
}

AND f_result() BE
{ LET a, s = 0, 0
  rega := lvofname(nameres, env)
  a := datav+rega
  IF rega=nilrv DO
  { rega := list(3, t_lvalue, rega)
    GOTO reserr
  }
  rega := h3!a
  a := datav+rega
  UNLESS h2!a=t_jj DO
reserr:	{ error("INCORRECT USE OF RES", 0, 0, 0)
	  errokdbg()
	  RETURN
	}
	s := datav+stack
  h4!s, h5!s, h6!s := h3!a, h4!a, h5!a
  f_return()
}

LET f_mult() BE
{ LET a, b, t = 0, 0, rega
  regb := pop(); rega := pop()
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := list(3, t_int, h3!a * h3!b )
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := list(3, t_real, h3!a #* h3!b )
    IF floterr DO ////??
    { writes("*nOVERFLOW:")
      floterr := FALSE
      GOTO fmuerr
    }
    push(rega); pc:=pc+1
    RETURN
  }
  rega := list(3, t_int, 0)
fmuerr:
  error1("**", t, regb)
  errdbg()
}

AND f_div() BE
{ LET a, b, t = 0, 0, rega
  regb := pop(); rega := pop()
  a := datav+rega
  b := datav+regb
  
  IF testnumbs2()=t_int DO
  { IF h3!b=0 GOTO derr
    rega := list(3, t_int, h3!a / h3!b )
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := list(3, t_real, h3!a #/ h3!b )
    IF floterr DO ////??
    { UNLESS h3!b #= 0.0 DO writes("*nOVERFLOW:")
      floterr := FALSE
      GOTO derr
    }
    push(rega); pc:=pc+1
    RETURN
  }
derr:
  rega := list(3, t_int, 0)
  error1("/", t, regb)
  errdbg()
}

AND f_plus() BE
{ LET a, b, t = 0, 0, ?
  regb := pop(); rega := pop()
  a := datav+rega
  b := datav+regb
  t := rega
                                 
  IF testnumbs2()=t_int DO
  { rega := list(3, t_int, h3!a + h3!b )
    push(rega); pc:=pc+1
    RETURN
  }

  IF testnumbs2()=t_real DO
  { rega := list(3, t_real, h3!a #+ h3!b )
    IF floterr DO
    { writes("*nOVERFLOW:")
      floterr := FALSE
      GOTO fperr
    }
    push(rega); pc:=pc+1
    RETURN
  }
  rega := list(3, t_int, 0)
fperr:
  error1("+", t, regb)
  errdbg()
}

AND f_minus() BE
{ LET a, b, t = 0, 0, rega
  regb := pop(); rega := pop()
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := list(3, t_int, h3!a - h3!b )
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := list(3, t_real, h3!a #- h3!b )
    IF floterr DO ////??
    { writes("*nOVERFLOW:")
      floterr := FALSE
      GOTO fmerr
    }
    push(rega); pc:=pc+1
    RETURN
  }
  rega := list(3, t_int, 0)
fmerr:
  error1("-", t, regb)
  errdbg()
}

AND f_power() BE
{ LET a, b, t = 0, 0, rega
  regb := pop(); rega := pop()
  a := datav+rega
  b := datav+regb
  UNLESS h2!b=t_int GOTO pwerr
  IF h2!a=t_int DO
  { LET base, exp, r = h3!a, h3!b, 1
    TEST exp <= 0
    THEN { IF base=0 GOTO pwerr
	   r := ABS base = 1 -> ((-exp & 1)=0 -> 1, base), 0
	 }
    ELSE UNTIL exp=0 DO
         { UNLESS (exp & 1)=0 DO r := r * base
	   base := base * base
	   exp := exp >> 1
	 }
    rega := list(3, t_int, r)
    push(rega); pc:=pc+1
    RETURN
  }
  IF h2!a=t_real DO
  { rega := list(3, t_real, fpower(h3!rega, h3!b) )
    IF floterr DO ////??
    { writes("*nOVERFLOW:")
      floterr := FALSE
      GOTO pwerr
    }
    push(rega); pc:=pc+1
    RETURN
  }
pwerr:
  rega := list(3, t_int, 0)
  error1("****", t, regb)
  errdbg()
}

AND f_pos() BE
{ LET a = 0
  rega := pop()
  a := datav+rega
  TEST h2!a=t_int | h2!a=t_real
  THEN { rega := list(3, h2!a, h3!a )
	 push(rega); pc:=pc+1
       }
  ELSE { error1("+", rega, 0)
	 rega := list(3, t_int, 0)
	 errdbg()
       }
}

AND f_neg() BE
{ LET a, t=0, rega
  rega := pop()
  a := datav+rega
  IF h2!a=t_int DO
  { rega := list(3, t_int, -h3!a )
    push(rega); pc:=pc+1
    RETURN
  }
  IF h2!rega=t_real DO
  { rega := list(3, t_real, #- h3!a )
    push(rega); pc:=pc+1
    RETURN
  }
  rega := list(3, t_int, 0)
  error1("-", t, 0)
  errdbg()
}

AND f_eq() BE
{ LET t=rega
  regb := pop(); rega := pop()
  rega := equal(rega, regb) -> truerv, falserv
  TEST errflag
  THEN { error1("EQ", t, regb)
	 errflag := FALSE
	 errdbg()
       }
  ELSE { push(rega); pc:=pc+1 }
}

AND f_ne() BE
{ LET t=rega
  regb := pop(); rega := pop()
  rega := equal(rega, regb) -> falserv, truerv
  TEST errflag
  THEN { error1("NE", t, regb)
	 rega := falserv
	 errflag := FALSE
	 errdbg()
       }
  ELSE { push(rega); pc:=pc+1 }
}

AND r_ls() BE
{ LET a, b = 0, 0
  regb := pop(); rega := pop()
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := h3!a < h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := h3!a #< h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  error1("LS", rega, regb)
  rega := falserv
  errdbg()
}

AND r_le() BE
{ LET a, b = 0, 0
  regb := pop(); rega := pop()
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := h3!a <= h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := h3!a #<= h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  error1("LE", rega, regb)
  rega := falserv
  errdbg()
}

AND r_ge() BE
{ LET a, b = 0, 0
  regb := pop; rega := pop()
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := h3!a >= h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := h3!a #>= h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  error1("GE", rega, regb)
  rega := falserv
  errdbg()
}

AND r_gr() BE
{ LET a, b = 0, 0
  regb := pop; rega := pop()
  a := datav+rega
  b := datav+regb
  IF testnumbs2()=t_int DO
  { rega := h3!a > h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  IF testnumbs2()=t_real DO
  { rega := h3!a #> h3!b -> truerv, falserv
    push(rega); pc:=pc+1
    RETURN
  }
  error1("GR", rega, regb)
  rega := falserv
  errdbg()
}

AND f_jump() BE
{ pc := pc!1
}

AND f_jumpF() BE
{ LET a = 0
  rega := pop()
  a := datav+rega
  IF h2!a = t_false DO
  { pc := pc!1
    RETURN
  }
  IF h2!a = t_true DO
  { pc := pc+2
    RETURN
  }
  error("NOT A TRUTHVALUE: ", rega, 0, 0)
  pc := pc!1 - 1
  edbg()
}

AND edbg() BE
{ restartc := pc+1
  pc := restartLoc
  rega := list(3, t_lvalue, nilrv)
  comdbg()
}

AND errdbg() BE
{ restartc := pc+1
  pc := codev+rvrestartLoc
  rega := list(3, t_lvalue, rega)
  prstate("From errdbg")
  //abort(1008)
  comdbg()
}

AND errlvdbg() BE
{ rega := list(3, t_lvalue, rega)
  errokdbg()
}

AND errokdbg() BE
{ restartc := pc+1
  pc := okrestartLoc
  comdbg()
}

AND comdbg() BE
{ LET b = 0
  LET s = datav+stack // Abs address of the current stack
//writef("*ncomdbg: entered, sp=%n stack=%n datav=%n*n", sp, stack, datav)
//abort(2000)
  h3!s := sp - s  // Rel address within the current stack
  regb := node(8)
  b := datav+regb // Abs address of regb
  h1!b := 8
  h2!b := t_stack
  h3!b := 0       // Location to hold the top of current stack
  h4!b := restartc
  h5!b := stack
  h6!b := env
  h7!b := rega
  stack := regb
  // errorlv is a Rel address to an Lvalue node, typically
  //         -> [3, Lvalue, rval]  rval is rel to datav
//  writef("comdbg: New stack created*n")
//  writef("comdbg: datav=%n errorlv=%n*n", datav, errorlv)
//  abort(2001)
  regb := h3!(datav+errorlv)
  b := datav+regb
  sp := datav + stack + (7-2)
  errct := errct + 1
  IF errct >= maxerr DO pc := codev+norestartLoc
  UNLESS h2!b = t_closure | h2!b=t_basicfn DO
  { UNLESS errct >= maxerr DO
    writes("EXECUTION RESUMED*n*n")
    RETURN
  }
  TEST h2!b=t_closure
  THEN { sp := sp+1
         sp!1 := errorlv
	 rega := regb
	 oldc, pc := pc, h4!b
       }
  ELSE { pc := pc-3
	 f_nil()
	 f_formLvalue()
	 (h3!b)()
       }
  restartc := 0
}

AND okrestart() BE
{ rega := sp!1
  restart()
  sp!2 := rega
  sp := sp+1
}

AND f_rvrestart() BE
{ LET a = 0
  writef("*nf_rvrestart:*n")
  abort(1000)
  rega := sp!1
  a := datav+rega
  restart()
  sp!2 := h3!a
  sp := sp+1
}

AND norestart() BE
{ writes("*nMAXIMUM NUMBER OF RUN-TIME ERRORS REACHED*n")
  terminate1()
}

AND f_apply() BE
{ LET a, b = 0, 0
//writef("f_apply: entered*n")
//abort(1000)
  rega := pop()
  a := datav+rega
  rega := h3!a
  a := datav+rega
  SWITCHON h2!a INTO
  { CASE t_closure:
      sp := sp+1
      oldc, pc := pc+1, h4!a
      RETURN
      
    CASE t_tuple:
      regb := sp!1
      b := datav+regb
      sp := sp-1
      regb := h3!b
      b := datav+regb
      UNLESS h2!b=t_int DO
      { error(0, rega, " APPLIED TO ", regb)
        UNLESS h3!a=0 DO rega := h4!a
        errlvdbg()
        RETURN
      }
      { LET n = h3!b
        TEST 1 <= n <= h3!a
        THEN { rega := a!(n+2)
	       push(rega); pc:=pc+1
	     }
        ELSE { error(0, rega, " APPLIED TO ", regb)
	       UNLESS h3!a=0 DO
	       TEST n >= 1
	       THEN rega := a!(h3!a+2)
	       ELSE rega := h4!a
	       errlvdbg()
	     }
	     RETURN
      }

    CASE t_basicfn:
      (h3!a)()
      RETURN

    DEFAULT:
      error("ATTEMPT TO APPLY ", rega," TO ",sp!1)
      edbg()
  }
}
 
AND f_save() BE
{ // rega -> [3, ?, newenv]
  // stack -> [upb, Stack, for sp, prevc, preve, arg]
  
  // sp holds the previous sp
  LET n = pc!1
  LET newstack = node(6+n) // Allocate a new stack of the right size
  LET s = datav + newstack // Abs address of the new stack
  //writef("f_save: Executing SAVE %n*n", n)
  sp := datav+stack+(7-2)
  regb  := newstack
  h1!s  := 6+n          // The size
  h2!s  := t_stack      // The node type
//h3!s  := ?            // Space to save this stack's saved sp  
  h4!s  := oldc         // The return address
  h5!s  := stack        // Save the revious stack
  h6!s  := env          // Save the previous environment
  h7!s  := sp!0         // 2nd from top of the previous stack

  h3!(datav+stack) := sp-stack   // Save the previous stack's sp  
  stack := newstack
  sp    := datav + stack + (7-2) // Abs address
  env   := h3!(datav+rega)
  pc    := pc+2
  //writef("Returning from f_save*n")
  //prstate("At end of save")
  //abort(1001)
}

AND f_return() BE
{ rega := sp!1
  restart()
  sp   := sp-1
  sp!1 := rega
}

AND f_testEmpty() BE
{ rega := pop()
  TEST h3!rega=nilrv
  THEN pc := pc+1
  ELSE { error1("FUNCTION OF NO ARGUMENTS", rega, 0)
	 edbg()
       }
}

AND f_lose1() BE
{ rega := pop()
  pc := pc+1
}

AND f_goto() BE
{ rega := pop()
  UNLESS h2!rega=t_label DO
  { error("CANNOT GO TO ", rega, 0, 0)
    rega := dummyrv
    errdbg()
    RETURN
  }
  // rega -> [ 6, t_label, supb, c, s, e ]
  pc, env := h4!rega, h6!rega
  stack := node(h3!rega)
  sp := stack + (6-2)
  h1!stack, h2!stack := h3!rega, stack
  rega := h5!rega
  h4!stack, h5!stack, h6!stack := h4!rega, h5!rega, h6!rega
}

AND f_update() BE
{ LET n = pc!1
  regb := pop; rega := pop()
  TEST n = 1
  THEN h3!regb := rega
  ELSE { UNLESS h2!rega = t_tuple & h3!rega = n DO
	 { error("CONFORMALITY ERROR IN ASSIGNMENT",0,0,0)
	   writes("THE VALUE OF THE RHS IS: ")
	   printa(rega, tupledepth)
	   newline()
	   writes("THE NUMBER OF VARIABLES ON THE LHS IS: ")
	   writen(n)
	   newline()
	   pc := pc + 1
	   rega := dummyrv
	   errdbg()
	   RETURN
	 }
	 regb := h3!regb
	 { LET v = VEC 100
	   FOR i=3 TO n+2 DO v!i := h3!(rega!i)
	   FOR i=3 TO n+2 DO h3!(regb!i) := v!i
	 }
       }
  rega := dummyrv
  pc := pc+1
  push(rega); pc:=pc+1
}

// XPAL2D
MANIFEST { lfield=#o177777; ndist=24 }

LET error(ms1, db1, ms2, db2) BE
{ writes("*n*nRUN TIME ERROR: ")
  UNLESS ms1 = 0 DO writes(ms1)
  UNLESS db1 = 0 DO printa(db1, tupledepth)
  UNLESS ms2 = 0 DO writes(ms2)
  UNLESS db2 = 0 DO printa(db2, tupledepth)
  newline()
}

AND error1(op, arg1, arg2) BE
{ // arg1 and arg2 are subscripts of datav
  writes("*n*nRUN TIME ERROR: ")
  writes(op)
  writes(" APPLIED TO ")
  //writef("*narg1=%n arg2=%n tupledepth=%n*n", arg1, arg2, tupledepth)
  printa(arg1, tupledepth)
  UNLESS arg2=0 DO
  { writes(" AND ")
    printa(arg2, tupledepth)
  }
  newline()
}

AND prvalue(x, depth) BE
{ // n is the nesting depth limit
  LET p= datav+x
  //writef("printb(%n)*n", x)
  IF x=0 DO
  { writef("Null")
    RETURN
  }
  SWITCHON h2!p INTO
  { DEFAULT:
      writef("*nprvalue: DEFAULT: x=%n  -> [ %n %n ... ]*n", x, h1!p, h2!p)
      RETURN
      
    CASE t_int:
      writen(h3!p)
      RETURN

    CASE t_real:
    { writef("%9.5f", h3!p)
      RETURN
    }

    CASE t_true:
    CASE t_false:
    CASE t_nil:
    CASE t_nils:
      writef("[%n,%s]", h1!p, opstr(h2!p))
      RETURN

    CASE t_stack:
      writef("[%n,%s,..]", h1!p, opstr(h2!p))
      RETURN

    CASE t_string:
      IF depth=0 DO
      { writef("-")
        RETURN
      }
      writef("[%n,%s,", h1!p, opstr(h2!p))
      prvalue(h3!p, depth-1)
      writef(",'%c']", h4!p)
      RETURN

    CASE t_env:
      IF depth=0 DO
      { writef("-")
        RETURN
      }
      writef("[%n,%s,%n.", h1!p, opstr(h2!p), h3!p)
      writef(",'%s',", strv+h4!p)
      prvalue(h5!p, depth-1)
      writef("]")
      RETURN

    CASE t_lvalue:
      writef("[%n,%s,", h1!p, opstr(h2!p))
      IF depth=0 DO
      { writef("..]")
        RETURN
      }
      //writef("depth=%n ", depth)
      prvalue(h3!p, depth-1)
      writef("]")
      RETURN
      
    CASE t_tuple:
    { LET n = h3!p
      IF n = 0 DO
      { writes("NIL")
        RETURN
      }
      IF depth=0 DO
      { writes("etc")
        RETURN
      }
      writef("[%n,%s,%n,", h1!p, opstr(h2!p), h3!p)
      FOR i = 3 TO n+1 DO
      { prvalue(p!i, depth-1)
        writes(", ")
      }
      prvalue(p!(n+2), depth-1)
      wrch(']')
      RETURN
    }

    CASE t_closure:
      writef("[%n, Closure. %n, %n, %n]", h1!p, h3!p, h4!p, h5!p)
      RETURN

    CASE t_basicfn:
      writef("[%n, Bfn. %n]", h1!p, h3!p)
      RETURN

    CASE t_label:
      writes("Label")
      RETURN

    CASE t_jj:
      writes("Env")
      RETURN

    CASE t_dummy:
      writes("Dummy")
      RETURN
  }
}

AND printb(x) BE
{ LET p= datav+x
  //writef("printb(%n)*n", x)
  IF x=0 RETURN
  SWITCHON h2!p INTO
  { DEFAULT:
      writef("DEFAULT: printb x=%n  -> [ %n %n ... ]*n", x, h1!p, h2!p)
      RETURN
      
    CASE t_int:
      writen(h3!p)
      RETURN

    CASE t_real:
    { writef("%9.5f", h3!p)
      RETURN
    }

    CASE t_string:
      wrch(h4!p)
      printb(h3!p)

    CASE t_nils:
      RETURN

    CASE t_tuple:
    { LET n = h3!p
      IF n = 0 DO
      { writes("NIL")
        RETURN
      }
      IF @ x > stackwarning DO
      { writes("( ETC )")
        RETURN
      }
      wrch('(')
      FOR i = 3 TO n+1 DO
      { printb(p!i)
        writes(", ")
      }
      printb(p!(n+2))
      wrch(')')
      RETURN
    }

    CASE t_true:
      writes("TRUE")
      RETURN

    CASE t_false:
      writes("FALSE")
      RETURN

    CASE t_lvalue:
      printb(h3!p)
      RETURN

    CASE t_closure:
    CASE t_basicfn:
      writes("$FUNCTION$")
      RETURN

    CASE t_label:
      writes("$LABEL$")
      RETURN

    CASE t_jj:
      writes("$ENVIRONMENT$")
      RETURN

    CASE t_dummy:
      writes("$DUMMY$")
      RETURN
  }
}

AND printa(x, n) BE
{ LET p = datav+x
  IF x=0 RETURN

  IF n <= 0 DO
  { writes(" ETC ")
    RETURN
  }
  
  SWITCHON h2!p INTO
  { DEFAULT:
      wrch(' ')
      printb(x)
      wrch(' ')
      RETURN

    CASE t_string:
    CASE t_nils:
      wrch('*'')
      printb(x)
      wrch('*'')
      RETURN

    CASE t_tuple:
    { LET m = h3!p
      IF m=0 DO
      { writes(" NIL ")
        RETURN
      }
      wrch('(')
      FOR i = 3 TO m+1 DO
      { printa(p!i, n-1)
        wrch(',')
      }
      printa(p!(m+2), n-1)
      wrch(')')
      RETURN
    }

    CASE t_lvalue:
      printa(h3!p, n)
      RETURN
  }
}

AND equal(a,b) = VALOF
{ LET btag = h2!b
  SWITCHON btag INTO
  { CASE t_true:
    CASE t_false:
    CASE t_int:
    CASE t_real:
    CASE t_string:
    CASE t_nils:
      SWITCHON h2!a INTO
      { CASE t_true:
          IF btag=t_true RESULTIS TRUE
          RESULTIS FALSE
        CASE t_false:
	  IF btag=t_false RESULTIS TRUE
          RESULTIS FALSE
        CASE t_int:
	  IF btag=t_int & h3!a=h3!b RESULTIS TRUE
          RESULTIS FALSE
        CASE t_real:
	  IF btag=t_real & h3!a=h3!b RESULTIS TRUE
          RESULTIS FALSE
        CASE t_string:
	  IF btag=t_string & h4!a=h4!b 	RESULTIS equal(h3!a,h3!b)
          RESULTIS FALSE
        CASE t_nils:
          IF btag=t_nils RESULTIS TRUE
          RESULTIS FALSE
      }
  }
  errflag := TRUE
  RESULTIS FALSE
}

AND testnumbs2() = h2!(datav+rega)=t_int  & h2!(datav+regb)=t_int  -> t_int,
		   h2!(datav+rega)=t_real & h2!(datav+regb)=t_real -> t_real,
		   t_false

AND testbools2() = ( h2!rega=t_true | h2!rega=t_false ) &
		   ( h2!regb=t_true | h2!regb=t_false )

AND lvofname(n, p) = VALOF
{ // n is the position in strv of a variable.
  // p is the rel address of the current evironment.
  //writef("lvofname: n=%n=%s p=%n*n", n, strv+n, p)
  h3!lookupno := h3!lookupno + 1
  UNTIL p = 0 DO
  { LET q = datav + p
    writef("lvofname: n=%s h4!q=%s p=%n*n", strv+n, strv+h4!q, p)
    //prstate("In lvoname")
    //writef("q -> [ %n %n %n %n %n ]*n", h1!q, (h2!q), h3!q, h4!q, h5!q)
    //abort(9766)
    IF h4!q = n DO
    { writef("lvofname: Returning %n*n", h5!q)
      //abort(9767)
      RESULTIS h5!q
    }
    writef("lvofname: No match at p=%n*n", p)
    p := h3!q
  }
  //writef("lvofname: Var %n not declared, strv=%n*n", n, strv)
  //abort(999)
  UNLESS n=nameres DO
    error("UNDECLARED NAME: ", 0, strv+n, 0)
  RESULTIS nilrv
}

AND nameoflv(l, p) = VALOF
{ UNTIL p=0 DO
  { IF h5!p=l RESULTIS h4!p
    p := h3!p
  }
  RESULTIS 0
}

AND restart() BE
{ LET p = datav + stack // Abs address of saved stack
  LET b = 0
  pc, regb, env := h4!p, h5!p, h6!p
  b := datav + regb
  stack := node(h1!b) // Rel to datav
  sp := datav + stack + h3!b 
  FOR i = 0 TO sp-stack+1 DO
    stack!i := regb!i
}

AND terminate() BE
{ listt := listt + 6 // CREATE EXTRA SPACE FOR FINAL DIAGNOSE
  f_diagnose()
  terminate1()
}

AND terminate1() BE
{ //control(output, 2)
  //writen(h3!lookupno)
  //writes(" LOOKUPS *T")
  writen(count)
  writes(" Instructions executed*n")
  //gcmark := gcmark >> 16
  //writen(gcmark)
  //writes(" GARBAGE COLLECTIONS*n")
  longjump(xpendlevel, xpend)
}

AND lastfn1(q) = VALOF
{ LET name, arg = 0, 0
  LET y, n = 0, 0
  IF h6!q=0 RESULTIS FALSE
  { y := h5!q
    n := h3!y
    TEST n>6
    THEN { name := y!(n-1)
           UNLESS name=nilrv DO
           { name := nameoflv(name, h6!q)
             IF name=0 DO name := "ANONYMOUS"
             arg := y!(n-2)
	   }
	 }
    ELSE name := nilrv
    q := y
    /////IF p=0 RESULTIS TRUE
    IF h6!q=0 RESULTIS FALSE
  } REPEATWHILE name=nilrv
  writes("AT THIS TIME, THE FUNCTION BEING EXECUTED IS: ")
  writes(name)
  writes("*nTHE ARGUMENT TO WHICH IT IS BEING APPLIED IS: ")
  printa(arg, tupledepth)
  newline()
  RESULTIS TRUE
}

AND writenode(n) BE
{ writen(n RSHIFT ndist)
  wrch('*T')
  writes(h4!rega)
  wrch('*T')
  printa(h5!rega, tupledepth)
  newline()
}

//>>> eject
// XPAL2E
MANIFEST { lfield=#o177777; mfield=#o77600000; gc1=#o200000 }

LET nextarea(n) BE
{ LET b = FALSE
  IF gcdbg DO writes("*n*nNEXTAREA RECLAIMATION PHASE*n")
  { UNLESS listp=listl DO h1!listp := listl - listp
    IF listl=listt DO
    { IF b DO
      { writes("*n*nRUN TIME SPACE EXHAUSTED*n")
        terminate()
      }
      mark()
      IF gcdbg DO writes("*nMARKLIST PREFORMED*n")
      listl, b := listv, TRUE
    }
    h1!listt := 0
    WHILE ( h1!listl & mfield ) = gcmark DO
      listl := listl + ( h1!listl & lfield )
    listp := listl
    h1!listt := gcmark
    UNTIL ( h1!listl & mfield ) = gcmark DO
      listl := listl + ( h1!listl & lfield )
    IF gcdbg DO
    { writes("*S*S")
      writen(listl-listp)
    }
  } REPEATWHILE listp+n >= listl
  IF gcdbg DO writes("*S*n")
  RETURN
}

AND marklist(x) BE
{
l:IF @ x > stackwarning DO
  { writes("*n*nMAXIMUM NODE DEPTH EXCEEDED*n")
    terminate()
  }
  IF x=0 RETURN
  IF ( h1!x & mfield ) = gcmark RETURN
  h1!x := h1!x & lfield | gcmark
  SWITCHON h2!x INTO
  { DEFAULT:
      writes("*n*nMARKLIST ERROR*n")
      writex(x); writes(" H1!X="); writex(h1!x)
      writes(" NODE TYPE IS "); writen(h2!x)
      writes("*S*n")
      RETURN

    CASE t_tuple:
      FOR i = 1 TO h3!x DO marklist(x!(i+2))
      RETURN

    CASE t_env:
      marklist(h5!x)
      x := (h3!x)
      GOTO l

    CASE t_stack:
      FOR i = 4 TO h3!x-1 DO marklist(x!i)
      RETURN

    CASE t_jj:
      marklist(h5!x)
      x := (h4!x)
      GOTO l

    CASE t_label:
      marklist(h6!x)
      x := (h5!x)
      GOTO l

    CASE t_lvalue:
    CASE t_closure:
    CASE t_string:
      x := (h3!x)
      GOTO l

    CASE t_int:
    CASE t_true:
    CASE t_false:
    CASE t_nil:
    CASE t_nils:
    CASE t_basicfn:
    CASE t_guess:
    CASE t_dummy:
    CASE t_real:
      RETURN
  }
}

AND mark() BE
{ gcmark := gcmark + gc1
  nset := FALSE
  IF ( gcmark & mfield ) = 0 DO
  { writes("*n*nMAXIMUM NUMBER OF ")
    writes("GARBAGE COLLECTIONS PERFORMED*n")
    terminate()
  }
  marklist(env)
  h3!stack := sp - stack
  marklist(stack)
  marklist(rega)
  marklist(regb)
  RETURN
}

AND list1(n, a, b, c, d, e, f) = VALOF
{ LET p = node(n)
  SWITCHON n INTO
  { DEFAULT:
    CASE 7: p!6 := f
    CASE 6: p!5 := e
    CASE 5: p!4 := d
    CASE 4: p!3 := c
    CASE 3: p!2 := b
    CASE 2: p!1 := a
    CASE 1: p!0 := n
  }
  FOR i = 7 TO n DO p!i := 0 // Pad with zeros 
  RESULTIS p
}

// XPAL2F
MANIFEST { lfield=#o177777 }

LET pop() = VALOF
{ sp   := sp-1
  RESULTIS sp!2
}

AND push(x) BE
{ sp   := sp+1
  sp!1 := x
}

AND f_declname() BE
{ env := list(5, t_env, env, pc!1, sp!1)
  sp := sp-1
  pc := pc + 2
}

AND f_declnames() BE
{ LET n = pc!1 // Number of variable names
  // the top of the stack is a tuple of initial values
  rega := pop()
  // rega 
  rega := h3!rega
  UNLESS h2!rega=t_tuple & h3!rega=n DO
  { error("CONFORMALITY ERROR IN DEFINITION", 0, 0, 0)
    nameerror(n,1)
    RETURN
  }
  FOR i = 2 TO n+1 DO r_name(i,1)
  pc := pc+2+n
}

AND f_initname() BE
{ sp := sp-1
  f_name(1,7)
  pc := pc+2
}

AND f_initnames() BE
{ LET n = pc!1
  rega := pop(); rega := h3!rega
  UNLESS h2!rega=t_tuple & h3!rega=n DO
  { error("CONFORMALITY ERROR IN RECURSIVE DEFINITION",0,0,0)
    nameerror(n,4)
    RETURN
  }
  FOR i = 2 TO n+1 DO r_name(i,4)
  pc := pc+2+n
}

AND r_name(i,p) BE abort(998)
AND f_name(i,p) BE
{ TEST p <= 3
  THEN env := list(5, t_env, env, pc!i,
                   p=1 -> rega!(i+1), list(3, t_lvalue, (p=2 -> rega, nilrv)) )
  ELSE { regb := lvofname(pc!i, env)
         IF regb=nilrv DO regb := list(3, t_lvalue, regb)
         SWITCHON p INTO
         { CASE 4: h3!regb := h3!(rega!(i+1)); RETURN
           CASE 5: h3!regb := rega; RETURN
           CASE 6: h3!regb := nilrv; RETURN
           CASE 7: h3!regb := h3!(sp!2); RETURN
	 }
       }
}

AND nameerror(n,p) BE
{ writes("THE NAMES BEING DECLARED ARE:*n")
  FOR i = 2 TO n+1 DO
  { writes(pc!i)
    newline()
  }
  writes("THE VALUE(S) PROVIDED ARE: ")
  printa(rega, tupledepth)
  newline()
  TEST h2!rega=t_tuple
  THEN { LET m=n
         IF m>h3!rega DO m := h3!rega
         FOR i = 2 TO m+1 DO r_name(i,p)
         FOR i = m+2 TO n+1 DO r_name(i,p+2)
       }
  ELSE { r_name(2,p+1)
         FOR i = 3 TO n+1 DO r_name(i,p+2)
       }
  pc := pc+n+1
  edbg()
}

AND decllabel() BE
{ rega := list(6, t_stack, 6, h4!stack, h5!stack, h6!stack)
  rega := list(6, t_label, h1!stack, pc!2, rega, env)
  rega := list(3, t_lvalue, rega)
  env := list(5, t_env, env, pc!1, rega)
  pc := pc + 3
}

AND setlabEs() BE
{ rega := env
  FOR i = 1 TO pc!1 DO
  { h6!(h3!(h5!rega)) := env
    rega := h3!rega
  }
  pc := pc + 2
}

AND f_blocklink() BE
{ // pc!1 it the return address label
  //      This is placed in oldc
  // Push nilrv onto the stack
  // rega is set to point to [ 3, Lvalue, env ]
  // Blocklink is always folloed by a Save instruction
  oldc := pc!1
  sp := sp+1
  sp!1 := nilrv
  rega := list(3, t_lvalue, env)
  pc := pc+2
}

AND reslink() BE
{ sp!2 := list(3, t_lvalue, nilrv)
  sp := sp+1
  f_blocklink()
}

AND f_setup() BE
{ // Initialise the runtime state
  // env must have been initilised with all the builtin functions.
  LET n = pc!1  // The number of locals used
  LET s = 0     // Abs address of stack
  writef("f_setup: entered*n")
  //abort(6521)
  // Allocate the outermost stack
  oldc  := finishLoc       // Rel pointer to Finish in codev
  stack := list(5+0,       // The size of the stack item
                t_stack,   // the type
		4,         // Space for saved sp
		oldc,      // the return address
		0,         // The previous stack -- none
		env)       // the previous env
  s := datav+stack
  sp := s + (6-2) // Abs pointer to top two elements
  FOR i = 6 TO 5+n DO s!i := 0 // Clear the user locations
  //env := 0
  prstate("Before mklvnode(env) in setup")
  rega := mklvnode(env) // Rel pointer
  push(rega)            // Push an argument holding the
                        // current environment onto the stack
  prstate("Before save() in setup")
  abort(9911)
  f_save()
  //rega := pop()
  //pc := pc+2  // Step over [SETUP n]
  //prstate("At end of setup)
}





// XPAL3 LAST MODIFIED ON FRIDAY, 12 JUNE 1970
// AT 5:37:38.68 BY R MABEE
//>>> filename "XPAL3"
//
//	***********
//	*         *
//	*  XPAL3  *
//	*         *
//	***********
//
//>>> GET "XPALHD"
//>>> eject
// XPAL3A
LET f_finish() BE
{ writes("*n*nEXECUTION FINISHED*n")
  terminate1()
}

AND f_print() BE
{ //writef("f_print: entered*n")
  //abort(1000)
  rega := pop()
  printb(rega)
  rega := dummyrv
  nextlv11()
}

AND f_userpage() BE
{ rega := pop()
  control(output, -1)
  rega := dummyrv
  nextlv11()
  terminate()
}

AND f_stem() BE
{ rega := pop()
  regb := h3!rega
  rega := nilsrv
  UNLESS h2!regb=t_string DO
  { error1("STEM", regb, 0)
    errlvdbg()
    RETURN
  }
  rega := list(4, t_string, rega, h4!regb )
  nextlv11()
}

AND f_stern() BE
{ rega := pop()
  rega := h3!rega
  UNLESS h2!rega=t_string DO
  { error1("STERN", rega, 0)
    rega := nilsrv
    errlvdbg()
    RETURN
  }
  rega := h3!rega
  nextlv11()
}

AND f_conc() BE
{ LET x, y = 0, 0
  LET v = VEC 512
  rega := h3!(sp!1)
  UNLESS h2!rega=t_tuple & h3!rega=2 DO
concerr:{ error1("CONC", rega, 0)
	  rega := pop()
	  rega := nilsrv
	  errlvdbg()
	  RETURN
	}
  x, y := h2!(h3!(h4!rega)), h2!(h3!(h5!rega))
  UNLESS ( x=t_string | x=t_nils ) &
	 ( y=t_string | y=t_nils ) GOTO concerr
  regb, x := h3!(h4!rega), 1
  UNTIL h2!regb = t_nils DO
  { v!x := h4!regb
    regb := h3!regb
    x := x+1
  }
  IF x=1 DO
  { regb := h3!(h5!rega)
    rega := pop()
    rega := regb
    nextlv11()
    RETURN
  }
  regb := list(4, t_string, 0, 0)/////v!i)
  rega := regb
  FOR i = 2 TO x-1 DO
  { h3!rega := list(4, t_string, 0, v!i)
    rega := h3!rega
  }
  h3!rega := h3!(h5!(h3!(sp!1)))
  rega := pop()
  rega := regb
  nextlv11()
}

AND f_atom() BE
{ rega := pop()
  SWITCHON h2!(h3!rega) INTO
  { CASE t_true:
    CASE t_false:
    CASE t_int:
    CASE t_real:
    CASE t_string:
    CASE t_nils:
      rega := truerv
      nextlv11()
      RETURN
  }
  rega := falserv
  nextlv11()}


AND f_null() BE
{ rega := pop()
  rega := h2!(h3!rega)=t_tuple & h3!(h3!rega)=0 -> truerv, falserv
  nextlv11()
}

AND f_length() BE
{ rega := pop()
  UNLESS h2!(h3!rega)=t_tuple DO
  { error1("ORDER", rega, 0)
    rega := list(3, t_int, 0)
    errlvdbg()
    RETURN
  }
  rega := list(3, t_int, h3!(h3!rega) )
  nextlv11()
}

AND f_istruthvalue() BE
{ rega := pop()
  SWITCHON h2!(h3!rega) INTO
  { CASE t_true:
    CASE t_false:
      rega := truerv
      nextlv11()
      RETURN
  }
  rega := falserv
  nextlv11()
}

AND f_isnumber() BE
{ rega := pop()
  rega := h2!(h3!rega)=t_int -> truerv, falserv
  nextlv11()
}

AND f_isstring() BE
{ rega := pop()
  SWITCHON h2!(h3!rega) INTO
  { CASE t_string:
    CASE t_nils:
      rega := truerv
      nextlv11()
      RETURN
  }
  rega := falserv
  nextlv11()
}

AND f_isfunction() BE
{ rega := pop()
  SWITCHON h2!(h3!rega) INTO
  { CASE t_closure:
    CASE t_basicfn:
      rega := truerv
      nextlv11()
      RETURN
  }
  rega := falserv
  nextlv11()
}

AND f_isenvironment() BE
{ rega := pop()
  rega := h2!(h3!rega)=t_jj -> truerv, falserv
  nextlv11()
}

AND f_islabel() BE
{ rega := pop()
  rega := h2!(h3!rega)=t_label -> truerv, falserv
  nextlv11()
}

AND f_istuple() BE
{ rega := pop()
  rega := h2!(h3!rega)=t_tuple -> truerv, falserv
  nextlv11()
}

AND f_isreal() BE
{ rega := pop()
  rega := h2!(h3!rega)=t_real -> truerv, falserv
  nextlv11()
}

AND f_isdummy() BE
{ rega := pop()
  rega := h2!(h3!rega)=t_dummy -> truerv, falserv
  nextlv11()
}

AND f_share() BE
{ rega := pop()
  rega := h3!rega
  UNLESS h2!rega=t_tuple & h3!rega=2 DO
  { error1("SHARE", rega, 0)
    rega := falserv
    errlvdbg()
    RETURN
  }
  rega := h4!rega=h5!rega -> truerv, falserv
  nextlv11()
}

//>>> eject
// XPAL3B
MANIFEST {	nfield=#o67700000000; n1=#o100000000 }

LET f_ston() BE
{ rega := pop()
  rega := h3!rega
  UNLESS h2!rega=t_string DO
  { error1("STOI", rega, 0)
    rega := list(3, t_int, 0)
    errlvdbg()
    RETURN
  }
  { LET regb = 0
    WHILE h2!rega=t_string DO
    { regb := regb*10 + h4!rega - '0'
      rega := h3!rega
    }
    rega := list(3, t_int, regb)
    nextlv11()
  }
}

AND f_cton() BE
{ rega := pop()
  rega := h3!rega
  UNLESS h2!rega=t_string LOGAND h2!(h3!rega)=t_nils DO
  { error1("CTOI", rega, 0)
    rega := list(3, t_int, 0)
    errlvdbg()
    RETURN
  }
  rega := list(3, t_int, h4!rega )
  nextlv11()
}

AND f_ntoc() BE
{ rega := pop()
  rega := h3!rega
  UNLESS h2!rega=t_int & h3!rega < 256 & h3!rega >= 0 DO
  { error1("ITOC", rega, 0)
    rega := nilsrv
    errlvdbg()
    RETURN
  }
  rega := list(4, t_string, nilsrv, h3!rega )
  nextlv11()
}

AND ntor() BE
{ rega := pop()
  rega := h3!rega
  UNLESS h2!rega=t_int DO
  { error1("ITOR", rega, 0)
    rega := list(3, t_real, 0)
    errlvdbg()
    RETURN
  }
  rega := list(3, t_real, f_itor(h3!rega) )
  nextlv11()
}

AND f_rton() BE
{ rega := pop()
  rega := h3!rega
  UNLESS h2!rega=t_real DO
  { error1("RTOI", rega, 0)
    rega := list(3, t_int, 0)
    errlvdbg()
    RETURN
  }
  rega := list(3, t_int, f_rtoi(h3!rega) )
  nextlv11()
}

AND rdchar() BE
{ // This reads the next character from the keyboard.
  // The characters of the current input line are in linev.
  // It normally returns a stringof length one, but when
  // eof is reached it returns a null string.
  // Re-implemented by MR 30/04/2024
  LET ch = rdch()
  rega := pop() // Discard the argument of READCH
  TEST ch=endstreamch
  THEN rega := nilsrv
  ELSE rega := list(4, t_string, nilsrv, ch)
  nextlv11()
}

/*
{ rega := pop() // Discard the argument of READCHAR
  rega := list(2, t_nils)
  IF linep>linet DO
  { UNLESS dataflag GOTO enddata
    IF ch='#' DO
    TEST dataflag
    THEN { dataflag := FALSE
           nextlv11() // VALUE OF NILS INDICATES EOD
           RETURN
	 }
    ELSE
enddata: { writes("*nEND OF DATA FILE ENCOUNTERED*n*n")
           terminate1()
	 }
    linet := linev
    linet!0 := ch
    UNTIL ch="*n" DO
    { readch(input, lvch)
      linet := linet + 1
      linet!0 := ch
    }
    readch(input, lvch)
    linep := linev
  }
  rega := list(4, t_string, rega, linep!0 )
  linep := linep + 1
  nextlv11()
}
*/

AND f_table() BE
{ rega := pop();
  rega := h3!rega
  UNLESS h2!rega = t_tuple & h3!rega = 2 DO
tablerr:{ error1("TABLE", rega, 0)
          rega := nilrv
          errlvdbg()
          RETURN
	}
  { LET n = h3!(h4!rega)
    UNLESS h2!n = t_int GOTO tablerr
    n := h3!n
    regb := h3!(h5!rega)
    rega := node(n+3)
    rega!0, rega!1, rega!2 := n+3, t_tuple, n
    FOR i = 3 TO n+2 DO
      rega!i := list(3, t_lvalue, regb)
    nextlv11()
  }
}

AND f_diagnose() BE
{ LET n, i = 0, 1000
  rega := sp!1
  sp!1 := list(3, t_lvalue, dummyrv) // RETURN VALUE
                                     //REPLACES ARGUMENT ON STACK
  pc := pc+1
  IF h2!(h3!rega)=t_int DO i := h3!(h3!rega)
  errorlv := list(3, t_lvalue, list(3, t_basicfn, lastfn) )
  IF nset DO // 2 SUCCESSIVE EXECUTIONS OF DIAGNOSE REQUIRE
             // AN INTERVENING MARKING PHASE
  { mark()
    listl := listv  // TAKE ADVANTAGE OF THE EXTRA
  }
  // MARKING PHASE
  nset := TRUE
  control(output, -1)
  writes("THE CURRENT ENVIRONMENT IS:*n*n")
  rega := env
  q := stack
  IF h4!stack=restartc DO // TRUE IFF CALL IS FROM COMDBG
    lastfn1(0)            // PEEL OFF TOP STACK NODE
l:writes("*TVARIABLE*TRVALUE*n*n")
  WHILE h4!rega ~= 0 DO
  { LET m = h1!rega & nfield
    TEST m ~= 0
    THEN { writenode(m)
           writes("ETC*n")
           BREAK
	 }
    ELSE { n := n+n1
           h1!rega := h1!rega | n
           writenode(n)
           rega := h3!rega
	 }
  }
  i := i-1
  rega := h6!q
  control(output, 3)
  UNLESS lastfn1(1) DO
fini:
  { control(output, -1)
    RETURN
  }
  IF i <= 0 GOTO fini
  writes("*n*nTHE ENVIRONMENT IN WHICH ")
  writes("THE ABOVE APPLICATION TAKES PLACE IS:*n*n")
  GOTO l
}

AND lastfn() BE
{ sp!1 := list(3, t_lvalue, dummyrv) // RETURN VALUE
  // REPLACES ARGUMENT ON STACK
  pc := pc+1
  control(output, 2)
  q := stack
  IF h4!stack=restartc DO // TRUE IFF CALL IS FROM COMDBG
    lastfn1(0) // PEEL OFF TOP STACK NODE
  UNLESS lastfn1(1) DO
    writes("ERROR OCCURRED IN OUTER LEVEL OF PROGRAM*n")
  control(output, 3) }

AND lookupine() BE
{ // The top of the stack is assumed to be the LV of a 2-tuple
  // ( PAL string os a variable name, a JJ node)
  rega := pop()
  rega := h3!rega
  UNLESS h2!rega = t_tuple & h3!rega = 2 DO
lerr: { error1("LOOKUPINE", rega, 0)
        rega := nilrv
        errlvdbg()
        RETURN
      }
  { LET x, i, l = h3!(h5!rega), 1, namechain
    LET vp = VEC 10
    LET v = VEC 40
    regb := h3!(h4!rega)
    UNLESS h2!regb=t_string & h2!x=t_jj GOTO lerr
    WHILE h2!regb=t_string DO
    { v!i := h4!regb
      regb := h3!regb
      i := i+1
    }
    v!0 := i-1
    packstring(v, vp)
    i := ( i-1 ) /bytesperword + 1
    UNTIL l=0 DO
    { LET v = l!1
      IF vp!0=v!0 DO
      { IF i=1 BREAK
        IF vp!1=v!1 DO
        { IF i=2 BREAK
          IF vp!2=v!2 DO
     {	IF i=3 BREAK
                IF vp!3=v!3 DO
		{ IF i=4 BREAK
                  IF vp!4=v!4 DO
		  { IF i=5 BREAK
		  }
		}
		l := l!0
              }
	    }
	TEST l=0
	THEN i := vp
	ELSE i := l!1 
	rega := lvofname(i, h5!x)
	TEST rega=nilrv
	THEN errlvdbg()
	ELSE { push(rega); pc:=pc+1 } }}}}

AND f_saveenv() BE
{ rega := pop()
  rega := list(5, t_jj, h4!stack, h5!stack, h6!stack )
  nextlv11()
}

AND garbcollect() BE
{ // This just copies all accessible data in datav into
  // a new selfexpading array. All accessible data is
  // referenced directly or indirectly from the following
  // roots which are all relative to datav:
  //     datav!1 to datav!10
  //     stack
  //     env
  //     rega
  //     regb
  // Whenever a node is copied its usb is to minus the
  // new location.
  
  prevdatav := datav // save the previous datav
  
  datavupb  := 0  // Reset palsxv
  datav     := 0

  FOR i = 1 TO 10 DO // Allocate elements 1 to 10
    sxvpush(palsxv, 0)
  FOR i = 1 TO 10 DO datav!i := gc(datav!i)
  stack := gc(stack)
  env   := gc(env)
  rega  := gc(rega)
  regb  := gc(regb)
}

AND gc(p) = VALOF
{ // p is relative to datav
  LET upb, op, newnode = 0, 0, 0
  UNLESS p RESULTIS 0
  p := prevdatav+p        // Convert p to absolute address in datav
  upb := h1!p
  IF upb<0 RESULTIS -upb  // Already copied, return the new location
  op := h2!p
  newnode := node(upb)    // Allocate the new node
  h1!p := -newnode
  
  h1!newnode := upb
  h2!newnode := op
  
  SWITCHON op INTO // All possible runtime node types 
  { DEFAULT:
      writef("*nSystem error in gc: op=%s*n", op)
      abort(999)
      RESULTIS newnode

    CASE t_dummy:        // [ 2, t_dummy ]
    CASE t_false:        // [ 2, t_false ]
    CASE t_guess:        // [ 2, t_guess ]
    CASE t_nil:          // [ 2, t_nil ]
    CASE t_nils:         // [ 2, t_nils ]
    CASE t_sys:          // [ 2, t_sys ]
    CASE t_true:         // [ 2, t_true ]
      RESULTIS newnode

    CASE t_basicfn:      // [ 3, t_basicfn, val ] 
    CASE t_int:          // [ 3, t_int,     val ] 
    CASE t_real:         // [ 3, t_real,    val ] 
      h3!newnode := h3!p      //val
      RESULTIS newnode
      
    CASE t_closure:      // [ 4, t_closure, e, c ]
      h3!newnode := gc(h3!p)   // e
      h4!newnode := h4!p       // c
      RESULTIS newnode
    
    CASE t_env:          // [ 5, t_env, link, varname, val ]
      h3!newnode := gc(h3!p)    // link
      h4!newnode := h4!p        // varname
      h5!newnode := gc(h5!p)    // val
      RESULTIS newnode
    
    CASE t_jj:           // [ 6, t_jj, c, s, e ] 
      h3!newnode := h3!p        // c
      h4!newnode := gc(h4!p)    // s
      h5!newnode := gc(h5!p)    // e
      RESULTIS newnode
    
    CASE t_label:        // [ 6, t_label, relsp, c, s, e ] 
      h3!newnode := h3!p        // relsp
      h4!newnode := h4!p        // c
      h5!newnode := gc(h5!p)    // s
      h6!newnode := gc(h6!p)    // e
      RESULTIS newnode

    CASE t_lvalue:       // [ 3, t_lvalue, val ] 
      h3!newnode := gc(h3!p)   // val
      RESULTIS newnode


    CASE t_string:       // [ 4, t_string, link, ch ]
      h3!newnode := gc(h3!p)    // link
      h4!newnode := h4!p        // ch
      RESULTIS newnode

    CASE t_stack:        // [ upb, t_stack, relsp, c, s, e, <locals> ]
      h3!newnode := h3!p                             // relsp
      h4!newnode := h4!p                             // c
      FOR i = 4 TO h3!p+1    DO newnode!i := gc(p!i) // s, e, <locals>
      FOR i = h3!p+2 TO upb  DO newnode!i := 0       // Clear others
      RESULTIS newnode
    
    CASE t_tuple:        // [ upb, t-tuple, n, <elements> ]
      h3!newnode := h3!p                          // n
      FOR i = 3 TO h3!p+2 DO newnode!i := gc(p!i) // <elements>
      RESULTIS newnode

  }
  RESULTIS 0  
}


