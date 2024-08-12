/*
When modifications have been completed this file will be
identical to cinterp.c.

This the Cintcode interpreter written in C with modifications
for the Cintpos system. Originally there were two interpreters,
one called interpreter defined by this file and the second 
called cintasm implemented in assembly language providing fewer
debugging aids and specifically aiming for higher performance.
There were assembly languge interpreter written for several
architectures but these are no longer maintained and are obsolete.
If the macro FASTyes is defined this file will define the function
cintasm that will run somewhat faster than the standard version
but with fewer debugging aids.

If RASTERINGyes is defined this file will define an alternative
version cintasm that can generate rastering data. It is the version
used in rastsys and will generate rastering data if the variable
tallylim is non zero. The rastering data it can then generate can
be converted by the rast2ps command to create Postscript pictures
of the execution history of a program.

If TARGET64 is defined the compiled interpreter will be for
64 bit Cintcode.

(c) Copyright:  Martin Richards  23 November 2019

History

08/11/2019
lastWp, lastWg and lastst have been renames syp, sysg ans sysst,
and are set by dosys when executing a Cintcos SYS instruction.
When not in dosys sysp is zero. When the Cintcode memory is
dumped by dumpmem, these variable are copied into the rootnode
in order to be visible to the commands dumpsy, dumppos, sysdebug
and posdebug. If a memory dump is made when executing a SYS 
instruction valid P and G pointers for the BCPL sys call will be
available. The treatment of st is still nder development. It
should probably be the setting of st at the time of the fault.

04/11/2019
Attempting to make cinterp.c and raster.c identical controlled
by whether RASTERINGyes is defined. This mainly affects whether
the macros Rb, Rh and Rw are defined with null values. When
RASTERINGyes is defined they generate rastering data.
This change is still under development.

03/09/2018

If FASTyes if defined, most of the debugging aids are disabled making it
functionally equivalent to the handwritten assembly code versions provided for
some architectures. These assembly language versions are now obsolete.

The fast version defines the function cintasm while the slow version defines
interpret.

09/04/10
Put time the time stamp (days, msecs) in the rootnode approx every msec.

*/

#include <stdio.h>

/* cintsys.h contains machine/system dependent #defines  */
#include "cintsys.h"

/*
This file is used to generate three versions of the Cintcode interpreter. If
FASTyes is defined, it is used to define fasterp, a fast verions of the
interpreter with most debugging aids turned off. If RASTERINGyes is defined, it
is used too define rasterp.o, a version that can generate rastering data. If
neither are defined it is used to create cinterp.o, the standard interpreter
withh all the debugging aids. Cintsys and Cintpos contain two interpreter
cinterp and one of fasterp or rasterp. Switching between these is by the
interpreter command.

FASTyes and RASTERINGyes are never both defined at the same time.
*/
  
#define PCCHKyes
#define TRACINGyes
#define TALLYyes
#define WATCHyes
#define MEMCHKyes
#define COUNTyes

#ifdef FASTyes

#undef PCCHKyes
#undef TRACINGyes
#undef TALLYyes
#undef WATCHyes
#undef MEMCHKyes
#undef COUNTyes

#endif

// Variables defined in cintsys or cintpos
extern BCPLWORD  result2;
extern int       tracing;
extern BCPLWORD  memupb;
extern BCPLWORD *tallyv;
extern BCPLWORD  tallyupb; // The upb of tallyv

UBCPLWORD memupbb;
UBCPLWORD tallylimb;

#ifdef RASTERINGyes

/* Upb of address references buffer */
#define UPB 50000

static FILE *rasterfile;
static BCPLWORD addr[UPB+1], addp;
static BCPLWORD count=1000, scale=12, fcounttrig;
BCPLWORD fcount;
static BCPLWORD sound=0;
static unsigned char bitpos;
static unsigned char bits;

extern char *b2c_str(BCPLWORD bstr, char *cstr);
extern char *osfname(char *name, char *osname);

static void wrline(void);
BCPLWORD setraster(BCPLWORD n, BCPLWORD val);
static void rasterpoint(BCPLWORD p);

#endif


#define Gn_currco      7
#define Gn_result2    10

/* CINTCODE function codes  */

#define F_0       0

#define F_fltop   1
#define F_brk     2
#define F_k0      0
#define F_lf     12
#define F_lm     14
#define F_lm1    15
#define F_l0     16
#define F_fhop   27
#define F_jeq    28

#define F_k      32
#define F_kh     33
#define F_kw     34
#define F_k0g    32
#define F_k0g1   (F_k0g+32)
#define F_k0gh   (F_k0g+64)
#define F_s0g    44
#define F_s0g1   (F_s0g+32)
#define F_s0gh   (F_s0g+64)
#define F_l0g    45
#define F_l0g1   (F_l0g+32)
#define F_l0gh   (F_l0g+64)
#define F_l1g    46
#define F_l1g1   (F_l1g+32)
#define F_l1gh   (F_l1g+64)
#define F_l2g    47
#define F_l2g1   (F_l2g+32)
#define F_l2gh   (F_l2g+64)
#define F_lg     48
#define F_lg1    (F_lg+32)
#define F_lgh    (F_lg+64)
#define F_sg     49
#define F_sg1    (F_sg+32)
#define F_sgh    (F_sg+64)
#define F_llg    50
#define F_llg1   (F_llg+32)
#define F_llgh   (F_llg+64)
#define F_ag     51
#define F_ag1    (F_ag+32)
#define F_agh    (F_ag+64)
#define F_mul    52
#define F_div    53
#define F_mod    54
#define F_xor    55
#define F_sl     56
#define F_ll     58
#define F_jne    60

#define F_llp    64
#define F_llph   65
#define F_llpw   66
#define F_add    84
#define F_sub    85
#define F_lsh    86
#define F_rsh    87
#define F_and    88
#define F_or     89
#define F_lll    90
#define F_jls    92

#define F_l      96
#define F_lh     97
#define F_lw     98
#define F_rv    116
#define F_rtn   123
#define F_jgr   124

#define F_lp    128
#define F_lph   129
#define F_lpw   130
#define F_lp0   128
#define F_sys   145
#define F_swb   146
#define F_swl   147
#define F_st    148
#define F_st0   148
#define F_stp0  149
#define F_goto  155
#define F_jle   156

#define F_sp    160
#define F_sph   161
#define F_spw   162
#define F_sp0   160
#define F_s0    176
#define F_xch   181
#define F_gbyt  182
#define F_pbyt  183
#define F_atc   184
#define F_atb   185
#define F_j     186
#define F_jge   188

#define F_ap    192
#define F_aph   193
#define F_apw   194
#define F_ap0   192

#define F_xpbyt 205
#define F_lmh   206
#define F_btc   207
#define F_nop   208
#define F_a0    208
#define F_rvp0  211
#define F_st0p0 216
#define F_st1p0 218

#define F_mw    223

#define F_a     224
#define F_ah    225
#define F_aw    226
#define F_l0p0  224
#define F_s     237
#define F_sh    238

#define F_mdiv  239
#define F_chgco 240
#define F_neg   241
#define F_not   242
#define F_l1p0  240
#define F_l2p0  244
#define F_l3p0  247 
#define F_l4p0  249

#define F_selld 254
#define F_selst 255
#define F_255   255

#define sf_none    0     // Assignment operators
#define sf_vecap   1
#define sf_fmul    2
#define sf_fdiv    3
#define sf_fmod    4
#define sf_fadd    5
#define sf_fsub    6
#define sf_mul     7
#define sf_div     8
#define sf_mod     9
#define sf_add    10
#define sf_sub    11
#define sf_lshift 12
#define sf_rshift 13
#define sf_logand 14
#define sf_logor  15
#define sf_eqv    16
#define sf_xor    17

/* The function interpret is designed to be separately compiled,
// and possibly implemented in assembly language.
//
// Unless either TRACINGyes or TALLYyes are defined, its only free
// variable is the function dosys(p, g).
//
// mem  is the pointer to the cintcode memory.
// regs is the position in the Cintcode memory where the initial
//      value of the Cintcode registers.
//
// interpret executes Cintcode instructions and returns with an
// integer result as follows:

//    -2      sys(Sys_dumpmem) cause a memory dump to DUMP.mem
//    -1 *    sys(Sys_setcount, val) called
//     0 *    sys(Sys_quit, 0) called
//     1      Non existent instruction
//     2      Brk instruction
//     3      Zero count
//     4      PC too large or negative
//     5      Division by zero
//    10      Cintasm single step trap
//    11      Contents of watch address has changed
//    12      Memory address too large or negative
//    13      SIGINT received
//    14      Unknown floating point operation
//    15
//    16      P pointer too large or negative
//     n      sys(Sys_quit, n) called
//
// On return the Cintcode registers are dumped back in the vector regs
*/

#if defined(FASTyes) || defined(RASTERINGyes)
extern BCPLWORD *watchaddr, watchval;
#else
BCPLWORD *watchaddr=0, watchval=0;
#endif


#ifdef RASTERINGyes
// Define the rastering macros
#define Rb(a) if (tallylimb) rasterpoint(a);
#define Rh(a) if (tallylimb) rasterpoint(a<<1);
#define Rw(a) if (tallylimb) rasterpoint(a<<B2Wsh);

#else

// Define empty rastering macros if RASTERINGtes is not defined.
#define Rb(a)
#define Rh(a)
#define Rw(a)

#endif

#define B (BP W)
#define SB (SBP W)
#define H (HP W)
#define SH (SHP W)

// GW gets a signed 32 bit value from Cintcode byte address x
// Originally on 64 bit BCPL this is unsigned but was changed
// to be sign extended on 10/09/2019. This is an incompatible
// change but does not affect a 32 bit compiler targetting
// 32 bit Cintcode, or a 64 bit compiler targeting 64 bit
// code. This change requires a change to bcplcgcin.b but the
// compiled code will be better for most negative numbers.
// The same change must be made to rasterp.b and cmpltest.b
// may need modification.
#ifdef BIGENDER
#define GH(x) ((WD B[x+0]<<8) | B[x+1])
#define GW(x) ((BCPLWORD)(BCPLINT32)\((((((WD B[x]<<8)|B[x+1])<<8)|B[x+2])<<8)|B[x+3]))
#else
#define GH(x) ((WD B[x+1]<<8) | B[x])
#define GW(x) ((BCPLWORD)(BCPLINT32)((((((WD B[x+3]<<8)|B[x+2])<<8)|B[x+1])<<8)|B[x]))
#endif

#if defined(FASTyes) || defined(RASTERINGyes)
int cintasm(BCPLWORD regs, BCPLWORD *mem)
#else
int interpret(BCPLWORD regs, BCPLWORD *mem)
#endif

{  BCPLWORD *W = mem;

   register int icount = W[rootnode+Rtn_icount];

   register BCPLWORD           a  = W[regs+0];
   register BCPLWORD           b  = W[regs+1];
   BCPLWORD                    c  = W[regs+2];
   BCPLWORD                    p  = W[regs+3]>>B2Wsh;
   BCPLWORD                    g  = W[regs+4]>>B2Wsh;
   BCPLWORD                    st = W[regs+5];
   register BCPLWORD           pc = W[regs+6];
   BCPLWORD                    count = W[regs+7];
   BCPLWORD                    mw = W[regs+8];

   register BCPLWORD *Wp  = W+p,    /* Optimise access to the stack */
                     *Wg  = W+g,    /* Optimise access to the global vector */
                     *Wg1 = W+g+256;

   BCPLWORD res, k, i;

   UBCPLWORD memupbb = memupb<<B2Wsh;

   Rw(regs+0); 
   Rw(regs+1); 
   Rw(regs+2); 
   Rw(regs+3); 
   Rw(regs+4); 
   Rw(regs+5); 
   Rw(regs+6); 
   Rw(regs+7); 
   Rw(regs+8); 

   /*   tracing = 1; */

fetchchk:
   
   // Check PC is in range after a jump.
   // Not done when in fasterp or rasterp.
#ifdef PCCHKyes
   if((UBCPLWORD)pc > memupbb) { res = 4; goto ret; }
#endif
   
fetch:

#ifdef WATCHyes
   /* Special watch debugging aid */
   if(watchaddr && *watchaddr!=watchval)
   { /*
       printf("%7ld: changed from %7ld (%8lX) to %7ld (%8lX)\n",
              (long)(watchaddr-W), (long)watchval, (long)(UBCPLWORD)watchval,
              (long)*watchaddr, (long)(UBCPLWORD)*watchaddr);
     */
     watchval = *watchaddr;
     W[1] = watchaddr-W;  /* Make watchaddr and watchval visible to debug */
     W[2] = watchval;
     res = 11;        /* Contents of watch address has changed */
     goto ret;
   }
   /* End of watch code */
#endif

   /* count>=0  means execute count instructions (slow interpreter)
      count=-1  means go on for ever (fast interpreter)
      count=-2  means single step the fast interpreter
   */
#ifdef COUNTyes
   if (count>=0)
   { if (count==0) { res = 3; goto ret; }
     count--;
   }
#endif

#ifdef TRACINGyes
   if (tracing) trace(pc, p, a, b);
#endif

#ifdef TALLYyes
   if ((UBCPLWORD)pc < tallylimb)
   { tallyv[pc]++;
     //printf("tallyv[%lld] = %lld\n", LL pc, LL tallyv[pc]);
   }
#endif

  if(--icount<=0) {
    // Try to set icount so that this code is executed about
    // 50 times per second (20msecs).
    // It sets the date and time in the rootnode and
    // if the joystick device is open it reads the joystick values
    // into the rootnode.
    int msecs = W[rootnode+Rtn_msecs];
    int previcount = icount;
    int corr = 0;
    int fd = W[rootnode+Rtn_joystickfd];

    // Update the days and msecs fields in the rootnode
    timestamp(&W[rootnode+Rtn_days]);
    corr = W[rootnode+Rtn_msecs] - msecs - 20; // Correction in msecs

    if (corr<-10) corr = -10;
    if (corr>+10) corr = +10;

    icount = W[rootnode+Rtn_icount]; // Estimate Cintcode instructions per sec

    if (corr<-3 || corr>+3)
      { icount -= (icount * corr) / 100;  // Note: -10 <= corr <= 10
      if (icount <     100000) icount =     100000;
      if (icount > 1000000000) icount = 1000000000;
      W[rootnode+Rtn_icount] = icount; // Update the estimate

      //printf("time diff=%d corr=%3d new icount=%d   \n",
      //     W[rootnode+Rtn_msecs]-msecs, corr, icount);
    }

    icount /= 50; // icount now is the estimated instruction count
                  // per 1/50 sec.


    //icount = 10000;
    // icount     bench100 time
    //   1      923.370
    //   10      99.780
    //   100     16.040
    //   1000     7.470
    //   10000    6.600 (gcc -O9)
    //   100000   6.510 (gcc -O3)
    //            5.550 without this code omitted (gcc -O1)
    //trpush(0x11000000+W[rootnode+Rtn_msecs]);

#ifdef JSAvail
    if (fd)
      { //printf("Calling joyscan(%d,...)\n", fd);
      joyscan(fd, &W[g], W);
    }
#endif
  }

// Uncommenting the next line increase the bench100 time
// from 6.310 to 7.180 and is not a very useful check.

//if((UBCPLWORD)p > memupbb) { res = 16; goto ret; }


#ifdef RASTERINGyes
fcount++;
#endif
  
Rb(pc);
switch(B[pc++])
{  default:
   case F_0:       // Cases F_0 and F_255 have been added explicitly to
   //case F_255:   // improve the compiled code (with luck).
   res = 1; pc--;  // Unimplemented instruction
   goto ret;

   // Added 21/7/10
   case F_fltop:
     { BCPLWORD op = B[pc++];

       //printf("fltop op=%ld\n", (long)op);
       switch (op) {
       default:
         W[1] = op;
         res = 14; goto ret;

       case fl_avail:
         a = -1; goto fetch;

       case fl_mk:
       { BCPLWORD exponent = B[pc++]; // Signed byte
	 // The senior bit represents -128
         if (exponent>=128) exponent = exponent-256;
	 //printf("fl_mk calling doflt(%ld, %ld, %ld)\n",
         //        (long)op, (long)a, (long)exponent);
         a = doflt(op, a, exponent, 0);
         goto fetch;
       }

       case fl_float:
       case fl_fix:
       case fl_pos:
       case fl_neg:
       case fl_abs:
         a = doflt(op, a, 0, 0);
         goto fetch;

       case fl_mul:
       case fl_div:
       case fl_mod: // Added 14/5/18
       case fl_add:
       case fl_sub:
       case fl_eq:
       case fl_ne:
       case fl_ls:
       case fl_gr:
       case fl_le:
       case fl_ge:
         //if(op==fl_ls)
	 //  printf("cinterp: FLT fl_ls instruction op=%d b=%8X a=%8X\n", op, b, a);
         a = doflt(op, b, a, 0);
         goto fetch;
       }
     }

     // Added 21/7/10
   case F_selld:  // load a field  SELLD len sh
     { BCPLWORD len = B[pc++];
       BCPLWORD sh  = B[pc++];
       BCPLWORD mask = -1;
       if (len) mask = (1<<len) - 1;
       //printf("%8x >> %d => %8x\n", W[a], sh, (UBCPLWORD)W[a]>>sh);
       a = ((UBCPLWORD)W[a]>>sh) & mask;
       goto fetch;
     }

     // Added 21/7/10
   case F_selst: // SLCT len:sh:0 OF <arg1> op:= <arg2>
                 //      len sh         a   op      b
     { BCPLWORD *ptr = &W[a];
       BCPLWORD op  = B[pc++];
       BCPLWORD len = B[pc++];
       BCPLWORD sh  = B[pc++];
       BCPLWORD mask;
       BCPLWORD val;
       BCPLWORD oldval, s;
       BCPLFLOAT t;

       if(len==0) {
         mask = UWD(-1) >> sh;
       } else {
         mask = (1<<len) - 1;
       }
       // Care needed because >> may be arithmetic not logical
       val = WD(((UWD*ptr)>>sh)) & mask;
       oldval = val; // Old value shifted down

       // val and oldval are both the old field value shifted down
       switch(op) {
       default:          a = 0; goto fetch;
       case sf_none:     val = b;                 break;
       case sf_vecap:    val = W[val + b];        break;
       case sf_fmul:     s = b;
	 //printf("val=%13.5f  b=%13.5f ", N2F(val), N2F(s));
                         t = N2F(val) * N2F(s);
	 //printf("=> %13.5f\n", t);
                         val = F2N(t);
                         break;
       case sf_fdiv:     s = b;
                         t = N2F(val) / N2F(s);
                         val = F2N(t);
                         break;
       case sf_fmod:     s = b;
	                 t = Cfmod(N2F(val), N2F(s));
                         val = F2N(t);
                         break;
       case sf_fadd:     s = b;
                         t = N2F(val) + N2F(s);
                         val = F2N(t);
                         break;
       case sf_fsub:     s = b;
                         t = N2F(val) - N2F(s);
                         val = F2N(t);
                         break;
       case sf_mul:      val *= b;                break;
       case sf_div:      val /= b;                break;
       case sf_mod:      val %= b;                break;
       case sf_add:      val += b;                break;
       case sf_sub:      val -= b;                break;

       // Negative shifts shift in the reverse direction and care
       // is needed because >> to ensure that vacated position
       // are filled with zeroes.
       case sf_lshift:   if (abs(b) >= BperW) val=0;
	                 if (b>=0) val <<= b;
	                 else      val = WD((UWD val)>>(-b));
                         break;
       case sf_rshift:   if (abs(b) >= BperW) val=0;
	                 if (b>=0) val = WD((UWD val)>>b);
	                 else      val <<= (-b);
                         break;

       case sf_logand:   val &= b;                break;
       case sf_logor:    val |= b;                break;
       case sf_eqv:      val = ~(val ^ b);        break;
       case sf_xor:      val ^= b;                break;
       }
       //printf("selst: op=%ld len=%ld sh=%ld "
       //       "oldval=%08lX val=%08lX mask=%08lX\n",
       //       (long)op, (long)len, (long)sh, (long) UWD oldval,
       //       (long)UWD (long)val, (long)UWD mask);
       // Replace field by new value
       *ptr ^= ((val ^ oldval)&mask) << sh;
       goto fetch;
     }

   case F_mul:   a = b * a;        goto fetch;
   case F_div:   if(a==0) {res=5;  goto ret; } /* Division by zero */
                 a = b / a;        goto fetch;
   case F_mod:   if(a==0) {res=5;  goto ret; } /* Division by zero */
                 a = b % a;        goto fetch;
   case F_add:   a = b + a;        goto fetch;
   case F_sub:   a = b - a;        goto fetch;
   case F_neg:   a = - a;          goto fetch;

   case F_fhop:  a = 0; pc++;      goto fetch;

   // Negative shifts shift in the reverse direction and care
   // is needed because >> to ensure that vacated position
   // are filled with zeroes.
   case F_lsh:   if (abs(a) >= BperW) { a=0; goto fetch; }
                 if (a>=0) a = b<<a;
                 else      a = WD((UWD b)>>(-a));
                 goto fetch;
   case F_rsh:   if (abs(a) >= BperW) { a=0; goto fetch; }
                 if (a>=0) a = WD((UWD b)>>a);
                 else      a = b<<(-a);
                 goto fetch;

   case F_not:   a = ~ a;          goto fetch;
   case F_and:   a = b & a;        goto fetch;
   case F_or:    a = b | a;        goto fetch;
   case F_xor:   a = b ^ a;        goto fetch;

   case F_goto:  pc = a;           goto fetchchk;

   case F_brk:   res = 2; pc--; goto ret;  /* BREAKPOINT  */
                 
   case F_rv+6:  Rw(a+6); a = W[a+6]; goto fetch;
   case F_rv+5:  Rw(a+5); a = W[a+5]; goto fetch;
   case F_rv+4:  Rw(a+4); a = W[a+4]; goto fetch;
   case F_rv+3:  Rw(a+3); a = W[a+3]; goto fetch;
   case F_rv+2:  Rw(a+2); a = W[a+2]; goto fetch;
   case F_rv+1:  Rw(a+1); a = W[a+1]; goto fetch;
   case F_rv:    Rw(a+0); a = W[a+0]; goto fetch;

   case F_st+3:  Rw(a+3); W[a+3] = b; goto fetch;
   case F_st+2:  Rw(a+2); W[a+2] = b; goto fetch;
   case F_st+1:  Rw(a+1); W[a+1] = b; goto fetch;
   case F_st:    Rw(a+0); W[a+0] = b; goto fetch;

   case F_chgco: Rw(p+0); Rw(g+Gn_currco); Rw(Wg[Gn_currco]); 
                 W[Wg[Gn_currco]] = Wp[0];     /* !currco := !p    */
                 Rw(p+1); 
                 pc = Wp[1];                   /* pc      := p!1   */
                 Rw(p+4); Rw(g+Gn_currco); 
                 Wg[Gn_currco] = Wp[4];        /* currco  := cptr  */
                 Rw(p+4); Rw(Wp[4]); 
                 p = W[Wp[4]]>>B2Wsh;              /* p       := !cptr */
                 Wp = W+p;
                 goto fetch;

   case F_mdiv:  Rw(p+3); Rw(p+4); Rw(p+5); 
                 a = muldiv(Wp[3], Wp[4], Wp[5]);
                 Rw(g+Gn_result2); 
                 Wg[Gn_result2] = result2;
                 /* fall through to return  */
   case F_rtn:   Rw(p+1); 
                 pc = Wp[1];
                 Rw(p+0); 
                 p  = Wp[0]>>B2Wsh;  Wp = W+p; goto fetch;

   case F_gbyt: Rb(a+(b<<B2Wsh));
                a = B[a+(b<<B2Wsh)];            goto fetch;
		
   case F_pbyt: Rb(a+(b<<B2Wsh));
                B[a+(b<<B2Wsh)] = c;            goto fetch;
		
   case F_xpbyt:Rb(b+(a<<B2Wsh));
                B[b+(a<<B2Wsh)] = c;            goto fetch;
		
   case F_atc:  c = a;                      goto fetch;
   case F_btc:  c = b;                      goto fetch;
   case F_atb:  b = a;                      goto fetch;
   case F_xch:  a = a^b; b = a^b; a = a^b;  goto fetch;

   case F_swb: { BCPLWORD n, k, val, i=1;
                 k = (pc+1)>>1;
                 Rh(k);
                 n = H[k];
                 while(i<=n)
                 { i += i;
                   val = H[k+i];
                   if (a==val) { k += i; break; }
                   if (a<val) i++;
                 }
                 k++;
                 Rh(k);
                 pc = (k<<1) + SH[k];
                 goto fetch;
               }

   case F_swl: { BCPLWORD n,q;
                 q = (pc+1)>>1;
                 Rh(q);
                 n = H[q++];
                 if(0<=a && a<n) q = q + a + 1;
                 Rh(q);
                 pc = (q<<1) + SH[q];
                 goto fetch;
               }

   case F_sys: switch(a) {
                 default: // system call -- general case
 
		         W[regs+0]  = a;    /* Save all the registers */
		         W[regs+1]  = b;    /* for debugging purposes */
                         W[regs+2]  = c;
                         W[regs+3]  = p<<B2Wsh;
                         W[regs+4]  = g<<B2Wsh;
                         W[regs+5]  = st;
                         W[regs+6]  = pc;
                         W[regs+7]  = count;
                         W[regs+8]  = mw;
  
                         a = dosys(p, g); 
                         goto fetch;

                 case Sys_setcount:
                         /* oldcount := sys(Sys_setcount, newcount)  */
                         a = count; 
		         Rw(p+4);
			 count = Wp[4];
                         res = -1; /* Leave and immediately re-enter */
                         goto ret; /* the interpreter */

                 case Sys_quit:
                         Rw(p+4); res = Wp[4];
                         goto ret;

		 /*
		 case Sys_rti: //  sys(Sys_rti, regs)
                 case Sys_saveregs: // sys(Sys_saveregs, regs)
                 case Sys_setst: // sys(Sys_setst, st)
              */

		case  Sys_tally:   // sys(Sys_tally, flag)
		         // This is implemented here so that the
		         // variable tallylimb private to cinterp
		         // can be inspected and updated efficiently. 
                         if(W[p+4]) {
	                   // Clear the tally vector and start tallying.
	                   // If in rastsys this will start outputting
	                   // raster or sound data.
                           tallylimb = tallyupb;
	                   printf("Sys_tally: start tallying, tallylimb=%lld\n",
		                   LL tallylimb);
                           for(i=1; i<=tallylimb; i++) tallyv[i] = 0;
                         } else {
	                   // Stop tallying or rastering, but leave the
			   // collected data in tallyv.
	                   if (tallylimb) printf("Sys_tally: Stop tallying\n");
                           tallylimb = 0;
                         }
                         goto fetch;
     
		 case Sys_watch:  /* sys(Sys_watch, addr) */
                       { watchaddr = &W[Wp[4]];
	                 watchval = *watchaddr;
                         goto fetch;
                       }
               }

   case F_lp0+16:  b = a; Rw(p+16); a = Wp[16]; goto fetch;
   case F_lp0+15:  b = a; Rw(p+15); a = Wp[15]; goto fetch;
   case F_lp0+14:  b = a; Rw(p+14); a = Wp[14]; goto fetch;
   case F_lp0+13:  b = a; Rw(p+13); a = Wp[13]; goto fetch;
   case F_lp0+12:  b = a; Rw(p+12); a = Wp[12]; goto fetch;
   case F_lp0+11:  b = a; Rw(p+11); a = Wp[11]; goto fetch;
   case F_lp0+10:  b = a; Rw(p+10); a = Wp[10]; goto fetch;
   case F_lp0+9:   b = a; Rw(p+9);  a = Wp[9];  goto fetch;
   case F_lp0+8:   b = a; Rw(p+8);  a = Wp[8];  goto fetch;
   case F_lp0+7:   b = a; Rw(p+7);  a = Wp[7];  goto fetch;
   case F_lp0+6:   b = a; Rw(p+6);  a = Wp[6];  goto fetch;
   case F_lp0+5:   b = a; Rw(p+5);  a = Wp[5];  goto fetch;
   case F_lp0+4:   b = a; Rw(p+4);  a = Wp[4];  goto fetch;
   case F_lp0+3:   b = a; Rw(p+3);  a = Wp[3];  goto fetch;

   case F_lp:   Rb(pc); Rw(p+B[pc]);
                b = a; a = Wp[B[pc++]];          goto fetch;
   case F_lph:  Rb(pc+1); Rw(p+GH(pc));
                b = a; a = Wp[GH(pc)];  pc += 2; goto fetch;
   case F_lpw:  Rb(pc+3); Rw(p+GW(pc));
                b = a; a = Wp[GW(pc)];  pc += 4; goto fetch;

   case F_llp:  b = a;  Rb(pc);   a = p+B[pc++];             goto fetch;
   case F_llph: b = a;  Rb(pc+1); a = p+GH(pc);     pc += 2; goto fetch;
   case F_llpw: b = a;  Rb(pc+3); a = p+GW(pc);     pc += 4; goto fetch;

   case F_sp0+16: Rw(p+16); Wp[16] = a; goto fetch;
   case F_sp0+15: Rw(p+15); Wp[15] = a; goto fetch;
   case F_sp0+14: Rw(p+14); Wp[14] = a; goto fetch;
   case F_sp0+13: Rw(p+13); Wp[13] = a; goto fetch;
   case F_sp0+12: Rw(p+12); Wp[12] = a; goto fetch;
   case F_sp0+11: Rw(p+11); Wp[11] = a; goto fetch;
   case F_sp0+10: Rw(p+10); Wp[10] = a; goto fetch;
   case F_sp0+9:  Rw(p+9);  Wp[9]  = a; goto fetch;
   case F_sp0+8:  Rw(p+8);  Wp[8]  = a; goto fetch;
   case F_sp0+7:  Rw(p+7);  Wp[7]  = a; goto fetch;
   case F_sp0+6:  Rw(p+6);  Wp[6]  = a; goto fetch;
   case F_sp0+5:  Rw(p+5);  Wp[5]  = a; goto fetch;
   case F_sp0+4:  Rw(p+4);  Wp[4]  = a; goto fetch;
   case F_sp0+3:  Rw(p+3);  Wp[3]  = a; goto fetch;

   case F_sp:    Rb(pc); Rw(p+B[pc]);
                 Wp[B[pc++]] = a;                  goto fetch;
   case F_sph:   Rb(pc+1); Rw(p+GH(pc));
                 Wp[GH(pc)]  = a;         pc += 2; goto fetch;
   case F_spw:   Rb(pc+3); Rw(p+GW(pc));
                 Wp[GW(pc)]  = a;         pc += 4; goto fetch;

   case F_lgh:   Rb(pc+1); Rw(g+GH(pc));
                 b = a; a = Wg[GH(pc)];   pc += 2; goto fetch;
   case F_lg1:   Rb(pc); Rw(g+256+B[pc]);
                 b = a; a = Wg1[B[pc++]];          goto fetch;
   case F_lg:    Rb(pc); Rw(g+B[pc]);
                 b = a; a = Wg[B[pc++]];           goto fetch;

   case F_sgh:   Rb(pc+1); Rw(g+GH(pc));
                 Wg[GH(pc)]   = a;        pc += 2; goto fetch;
   case F_sg1:   Rb(pc); Rw(g+256+B[pc]);
                 Wg1[B[pc++]] = a;                 goto fetch;
   case F_sg:    Rb(pc); Rw(g+B[pc]);
                 Wg[B[pc++]]  = a;                 goto fetch;

   case F_llgh: b = a; Rb(pc+1); a = g+GH(pc);    pc += 2; goto fetch;
   case F_llg1: b = a; Rb(pc); a = g+256+B[pc++];          goto fetch;
   case F_llg:  b = a; Rb(pc); a = g+B[pc++];              goto fetch;

   case F_ll+1: Rb(pc); i = (pc>>1) + B[pc];
                Rh(i+1);  i = (i<<1) + SH[i];
                Rw(i>>B2Wsh); b = a; a = W[i>>B2Wsh]; pc++; goto fetch;

   case F_ll:   Rb(pc); Rw((pc+SB[pc])>>B2Wsh);
                b = a; a = W[(pc+SB[pc])>>B2Wsh];pc++;      goto fetch;

   case F_sl+1: Rb(pc); i = (pc>>1) + B[pc];
                Rh(i+1); i = (i<<1) + SH[i];
                Rw(i>>B2Wsh); W[i>>B2Wsh] = a;        pc++; goto fetch;

   case F_sl:   Rb(pc); Rw((pc+SB[pc])>>B2Wsh);
                W[(pc+SB[pc])>>B2Wsh] = a;            pc++; goto fetch;
   
   case F_lll+1:Rb(pc); i = (pc>>1) + B[pc];
                Rh(i+1); i = (i<<1) + SH[i];
                b = a; a = i>>B2Wsh;                  pc++; goto fetch;

   case F_lll:  Rb(pc); b = a; a = (pc+SB[pc])>>B2Wsh; pc++; goto fetch;
   
   case F_l0+10: b = a; a = 10; goto fetch;
   case F_l0+9:  b = a; a =  9; goto fetch;
   case F_l0+8:  b = a; a =  8; goto fetch;
   case F_l0+7:  b = a; a =  7; goto fetch;
   case F_l0+6:  b = a; a =  6; goto fetch;
   case F_l0+5:  b = a; a =  5; goto fetch;
   case F_l0+4:  b = a; a =  4; goto fetch;
   case F_l0+3:  b = a; a =  3; goto fetch;
   case F_l0+2:  b = a; a =  2; goto fetch;
   case F_l0+1:  b = a; a =  1; goto fetch;
   case F_l0:    b = a; a =  0; goto fetch;
   case F_l0-1:  b = a; a = -1; goto fetch; 

   case F_l:     Rb(pc); b = a; a = B[pc++];               goto fetch;
   case F_lh:    Rb(pc+1); b = a; a = GH(pc);     pc += 2; goto fetch;
   case F_lw:    Rb(pc+3); b = a; a = GW(pc);     pc += 4; goto fetch;

   case F_lm:    Rb(pc);   b = a; a = - WD(B[pc++]);         goto fetch;
   case F_lmh:   Rb(pc+1); b = a; a = - WD(GH(pc)); pc += 2; goto fetch;
                
   case F_lf+1:  b = a;
                 Rb(pc); a = (pc>>1) + B[pc];
                 Rh(a+1);  a = (a<<1) + SH[a];         pc++; goto fetch;

   case F_lf:    Rb(pc); b = a; a = pc + SB[pc];       pc++; goto fetch;
 
   case F_k0gh+11: Rw(p+11); Wp[11] = p<<B2Wsh; p += 11; goto applygh;
   case F_k0gh+10: Rw(p+10); Wp[10] = p<<B2Wsh; p += 10; goto applygh;
   case F_k0gh+9:  Rw(p+9);  Wp[ 9] = p<<B2Wsh; p +=  9; goto applygh;
   case F_k0gh+8:  Rw(p+8);  Wp[ 8] = p<<B2Wsh; p +=  8; goto applygh;
   case F_k0gh+7:  Rw(p+7);  Wp[ 7] = p<<B2Wsh; p +=  7; goto applygh;
   case F_k0gh+6:  Rw(p+6);  Wp[ 6] = p<<B2Wsh; p +=  6; goto applygh;
   case F_k0gh+5:  Rw(p+5);  Wp[ 5] = p<<B2Wsh; p +=  5; goto applygh;
   case F_k0gh+4:  Rw(p+4);  Wp[ 4] = p<<B2Wsh; p +=  4; goto applygh;
   case F_k0gh+3:  Rw(p+3);  Wp[ 3] = p<<B2Wsh; p +=  3;
   applygh:        Wp    = W+p;
                   Rw(p+1); Wp[1] = pc + 2;
                   Rb(pc+1); Rw(g+GH(pc)); pc = Wg[GH(pc)];
                   Rw(p+2); Wp[2] = pc;
                   Rw(p+3); Wp[3] =  a;
                   goto fetchchk;

   case F_k0g1+11: Rw(p+11); Wp[11] = p<<B2Wsh; p += 11; goto applyg1;
   case F_k0g1+10: Rw(p+10); Wp[10] = p<<B2Wsh; p += 10; goto applyg1;
   case F_k0g1+9:  Rw(p+9);  Wp[ 9] = p<<B2Wsh; p +=  9; goto applyg1;
   case F_k0g1+8:  Rw(p+8);  Wp[ 8] = p<<B2Wsh; p +=  8; goto applyg1;
   case F_k0g1+7:  Rw(p+7);  Wp[ 7] = p<<B2Wsh; p +=  7; goto applyg1;
   case F_k0g1+6:  Rw(p+6);  Wp[ 6] = p<<B2Wsh; p +=  6; goto applyg1;
   case F_k0g1+5:  Rw(p+5);  Wp[ 5] = p<<B2Wsh; p +=  5; goto applyg1;
   case F_k0g1+4:  Rw(p+4);  Wp[ 4] = p<<B2Wsh; p +=  4; goto applyg1;
   case F_k0g1+3:  Rw(p+3);  Wp[ 3] = p<<B2Wsh; p +=  3;
   applyg1:        Wp    = W+p;
                   Rw(p+1); Wp[1] = pc + 1;
                   Rb(pc); Rw(g+256+B[pc]); pc    = Wg1[B[pc]];
                   Rw(p+2); Wp[2] = pc;
                   Rw(p+3); Wp[3] = a;
                   goto fetchchk;
 
   case F_k0g+11: Rw(p+11); Wp[11] = p<<B2Wsh; p += 11; goto applyg;
   case F_k0g+10: Rw(p+10); Wp[10] = p<<B2Wsh; p += 10; goto applyg;
   case F_k0g+9:  Rw(p+9);  Wp[ 9] = p<<B2Wsh; p +=  9; goto applyg;
   case F_k0g+8:  Rw(p+8);  Wp[ 8] = p<<B2Wsh; p +=  8; goto applyg;
   case F_k0g+7:  Rw(p+7);  Wp[ 7] = p<<B2Wsh; p +=  7; goto applyg;
   case F_k0g+6:  Rw(p+6);  Wp[ 6] = p<<B2Wsh; p +=  6; goto applyg;
   case F_k0g+5:  Rw(p+5);  Wp[ 5] = p<<B2Wsh; p +=  5; goto applyg;
   case F_k0g+4:  Rw(p+4);  Wp[ 4] = p<<B2Wsh; p +=  4; goto applyg;
   case F_k0g+3:  Rw(p+3);  Wp[ 3] = p<<B2Wsh; p +=  3;
   applyg:         Wp    = W+p;
                   Rw(p+1); Wp[1] = pc + 1;
                   Rb(pc); Rw(g+B[pc]); pc    = Wg[B[pc]];
                   Rw(p+2); Wp[2] = pc;
                   Rw(p+3); Wp[3] = a;
                   goto fetchchk;
 
   case F_k0+11:  Rw(p+11); Wp[11] = p<<B2Wsh; p += 11; goto applyk;
   case F_k0+10:  Rw(p+10); Wp[10] = p<<B2Wsh; p += 10; goto applyk;
   case F_k0+9:   Rw(p+9);  Wp[ 9] = p<<B2Wsh; p +=  9; goto applyk;
   case F_k0+8:   Rw(p+8);  Wp[ 8] = p<<B2Wsh; p +=  8; goto applyk;
   case F_k0+7:   Rw(p+7);  Wp[ 7] = p<<B2Wsh; p +=  7; goto applyk;
   case F_k0+6:   Rw(p+6);  Wp[ 6] = p<<B2Wsh; p +=  6; goto applyk;
   case F_k0+5:   Rw(p+5);  Wp[ 5] = p<<B2Wsh; p +=  5; goto applyk;
   case F_k0+4:   Rw(p+4);  Wp[ 4] = p<<B2Wsh; p +=  4; goto applyk;
   case F_k0+3:   Rw(p+3);  Wp[ 3] = p<<B2Wsh; p +=  3;
   applyk:         Wp    = W+p;
                   Rw(p+1); Wp[1] = WD pc;
                   pc    = a;
                   Rw(p+2); Wp[2] = pc;
                   Rw(p+3); Wp[3] = a = b;
                   goto fetchchk;

   case F_k:       Rb(pc); k = B[pc]; Rw(p+k); Wp[k] = p<<B2Wsh; p +=  k;
                   Wp    = W+p;
                   Rw(p+1); Wp[1] = pc + 1;
                   pc    = a;
                   Rw(p+2); Wp[2] = pc;
                   Rw(p+3); Wp[3] = a = b;
                   goto fetchchk;

   case F_kh:      Rb(pc+1); k = GH(pc); Rw(p+k); Wp[k] = p<<B2Wsh; p +=  k;
                   Wp    = W+p;
                   Rw(p+1); Wp[1] = pc + 2;
                   pc    = a;
                   Rw(p+2); Wp[2] = pc;
                   Rw(p+3); Wp[3] = a = b;
                   goto fetchchk;

   case F_kw:      Rb(pc+3); k = GW(pc); Rw(p+k); Wp[k] = p<<B2Wsh; p +=  k;
                   Wp    = W+p;
                   Rw(p+1); Wp[1] = pc + 4;
                   pc    = a;
                   Rw(p+2); Wp[2] = pc;
                   Rw(p+3); Wp[3] = a = b;
                   goto fetchchk;

   case F_jeq:     if(b==a) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jeq+1:   if(b==a) goto indjump;
                   pc++; goto fetch;
   case F_jeq+2:   if(a==0) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jeq+3:   if(a==0) goto indjump;
                   pc++; goto fetch;

   case F_jne:     if(b!=a) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jne+1:   if(b!=a) goto indjump;
                   pc++; goto fetch;
   case F_jne+2:   if(a!=0) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jne+3:   if(a!=0) goto indjump;
                   pc++; goto fetch;

   case F_jls:     if(b<a) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jls+1:   if(b<a) goto indjump;
                   pc++; goto fetch;
   case F_jls+2:   if(a<0) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jls+3:   if(a<0) goto indjump;
                   pc++; goto fetch;

   case F_jgr:     if(b>a) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jgr+1:   if(b>a) goto indjump;
                   pc++; goto fetch;
   case F_jgr+2:   if(a>0) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jgr+3:   if(a>0) goto indjump;
                   pc++; goto fetch;

   case F_jle:     if(b<=a) {Rb(pc);  pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jle+1:   if(b<=a) goto indjump;
                   pc++; goto fetch;
   case F_jle+2:   if(a<=0) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jle+3:   if(a<=0) goto indjump;
                   pc++; goto fetch;

   case F_jge:     if(b>=a) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jge+1:   if(b>=a) goto indjump;
                   pc++; goto fetch;
   case F_jge+2:   if(a>=0) { Rb(pc); pc += SB[pc];   goto fetch; }
                   pc++; goto fetch;
   case F_jge+3:   if(a>=0) goto indjump;
                   pc++; goto fetch;

   case F_j:       Rb(pc); pc += SB[pc];        goto fetch;

 indjump:
   case F_j+1:     Rb(pc); pc = (pc>>1) + B[pc];
                   Rh(p+1c); pc = (pc<<1) + SH[pc];
                   goto fetch;

   case F_ap0+12:  Rw(p+12); a = a + Wp[12]; goto fetch;
   case F_ap0+11:  Rw(p+11); a = a + Wp[11]; goto fetch;
   case F_ap0+10:  Rw(p+10); a = a + Wp[10]; goto fetch;
   case F_ap0+9:   Rw(p+9);  a = a + Wp[ 9]; goto fetch;
   case F_ap0+8:   Rw(p+8);  a = a + Wp[ 8]; goto fetch;
   case F_ap0+7:   Rw(p+7);  a = a + Wp[ 7]; goto fetch;
   case F_ap0+6:   Rw(p+6);  a = a + Wp[ 6]; goto fetch;
   case F_ap0+5:   Rw(p+5);  a = a + Wp[ 5]; goto fetch;
   case F_ap0+4:   Rw(p+4);  a = a + Wp[ 4]; goto fetch;
   case F_ap0+3:   Rw(p+3);  a = a + Wp[ 3]; goto fetch;

   case F_ap:    Rb(pc);   Rw(p+B[pc]);     a += Wp[B[pc++]];         goto fetch;
   case F_aph:   Rb(pc+1); Rw(p+GH(pc));    a += Wp[GH(pc)]; pc += 2; goto fetch;
   case F_apw:   Rb(pc+3); Rw(p+GW(pc));    a += Wp[GW(pc)]; pc += 4; goto fetch;
   case F_agh:   Rb(pc+1); Rw(g+GH(pc));    a += Wg[GH(pc)]; pc += 2; goto fetch;
   case F_ag1:   Rb(pc);   Rw(g+256+B[pc]); a += Wg1[B[pc++]];        goto fetch;
   case F_ag:    Rb(pc);   Rw(g+B[pc]);     a += Wg[B[pc++]];         goto fetch;

   case F_a0+5: a += 5; goto fetch;
   case F_a0+4: a += 4; goto fetch;
   case F_a0+3: a += 3; goto fetch;
   case F_a0+2: a += 2; goto fetch;
   case F_a0+1: a += 1; goto fetch;
   case F_nop:          goto fetch;

   case F_a:    Rb(pc);   a += B[pc++];           goto fetch;
   case F_ah:   Rb(pc+1); a += GH(pc);   pc += 2; goto fetch;
   case F_aw:   Rb(pc+3); a += GW(pc);   pc += 4; goto fetch;
   case F_s:    Rb(pc);   a -= B[pc++];           goto fetch;
   case F_sh:   Rb(pc+1); a -= GH(pc);   pc += 2; goto fetch;

   case F_s0+4: a -= 4; goto fetch;
   case F_s0+3: a -= 3; goto fetch;
   case F_s0+2: a -= 2; goto fetch;
   case F_s0+1: a -= 1; goto fetch;

   case F_l0p0+12: Rw(p+12); Rw(Wp[12]+0);
                   b = a; a = W[Wp[12]+0]; goto fetch;
   case F_l0p0+11: Rw(p+11); Rw(Wp[11]+0);
                   b = a; a = W[Wp[11]+0]; goto fetch;
   case F_l0p0+10: Rw(p+10); Rw(Wp[10]+0);
                   b = a; a = W[Wp[10]+0]; goto fetch;
   case F_l0p0+9:  Rw(p+9);   Rw(Wp[9]+0);
                   b = a; a = W[Wp[ 9]+0]; goto fetch;
   case F_l0p0+8:  Rw(p+8);   Rw(Wp[8]+0);
                   b = a; a = W[Wp[ 8]+0]; goto fetch;
   case F_l0p0+7:  Rw(p+7);   Rw(Wp[7]+0);
                   b = a; a = W[Wp[ 7]+0]; goto fetch;
   case F_l0p0+6:  Rw(p+6);   Rw(Wp[6]+0);
                   b = a; a = W[Wp[ 6]+0]; goto fetch;
   case F_l0p0+5:  Rw(p+5);   Rw(Wp[5]+0);
                   b = a; a = W[Wp[ 5]+0]; goto fetch;
   case F_l0p0+4:  Rw(p+4);   Rw(Wp[4]+0);
                   b = a; a = W[Wp[ 4]+0]; goto fetch;
   case F_l0p0+3:  Rw(p+3);   Rw(Wp[3]+0);
                   b = a; a = W[Wp[ 3]+0]; goto fetch;

   case F_l1p0+6:  Rw(p+6);   Rw(Wp[6]+1);
                   b = a; a = W[Wp[ 6]+1]; goto fetch;
   case F_l1p0+5:  Rw(p+5);   Rw(Wp[5]+1);
                   b = a; a = W[Wp[ 5]+1]; goto fetch;
   case F_l1p0+4:  Rw(p+4);   Rw(Wp[4]+1);
                   b = a; a = W[Wp[ 4]+1]; goto fetch;
   case F_l1p0+3:  Rw(p+3);   Rw(Wp[3]+1);
                   b = a; a = W[Wp[ 3]+1]; goto fetch;

   case F_l2p0+5:  Rw(p+5);   Rw(Wp[5]+2);
                   b = a; a = W[Wp[ 5]+2]; goto fetch;
   case F_l2p0+4:  Rw(p+4);   Rw(Wp[4]+2);
                   b = a; a = W[Wp[ 4]+2]; goto fetch;
   case F_l2p0+3:  Rw(p+3);   Rw(Wp[3]+2);
                   b = a; a = W[Wp[ 3]+2]; goto fetch;

   case F_l3p0+4:  Rw(p+4);   Rw(Wp[4]+3);
                   b = a; a = W[Wp[ 4]+3]; goto fetch;
   case F_l3p0+3:  Rw(p+3);   Rw(Wp[3]+3);
                   b = a; a = W[Wp[ 3]+3]; goto fetch;

   case F_l4p0+4:  Rw(p+4);   Rw(Wp[4]+4);
                   b = a; a = W[Wp[ 4]+4]; goto fetch;
   case F_l4p0+3:  Rw(p+3);   Rw(Wp[3]+4);
                   b = a; a = W[Wp[ 3]+4]; goto fetch;

   case F_l0gh:  Rb(pc+1); Rw(g+GH(pc)); Rw(Wg[GH(pc)]+0);
                 b = a; a = W[Wg[GH(pc)]+0]; pc += 2; goto fetch;
   case F_l1gh:  Rb(pc+1); Rw(g+GH(pc)); Rw(Wg[GH(pc)]+1);
                 b = a; a = W[Wg[GH(pc)]+1]; pc += 2; goto fetch;
   case F_l2gh:  Rb(pc+1); Rw(g+GH(pc)); Rw(Wg[GH(pc)]+2);
                 b = a; a = W[Wg[GH(pc)]+2]; pc += 2; goto fetch;
   case F_l0g1:  Rb(pc);   Rw(g+256+B[pc]); Rw(Wg1[B[pc]]+0);
                 b = a; a = W[Wg1[B[pc++]]+0];        goto fetch;
   case F_l1g1:  Rb(pc);   Rw(g+256+B[pc]); Rw(Wg1[B[pc]]+1);
                 b = a; a = W[Wg1[B[pc++]]+1];        goto fetch;
   case F_l2g1:  Rb(pc);   Rw(g+256+B[pc]); Rw(Wg1[B[pc]]+2);
                 b = a; a = W[Wg1[B[pc++]]+2];        goto fetch;
   case F_l0g:   Rb(pc);   Rw(g+B[pc]); Rw(Wg[B[pc]]+0);
                 b = a; a = W[Wg[B[pc++]]+0];         goto fetch;
   case F_l1g:   Rb(pc);   Rw(g+B[pc]); Rw(Wg[B[pc]]+1);
                 b = a; a = W[Wg[B[pc++]]+1];         goto fetch;
   case F_l2g:   Rb(pc); Rw(g+B[pc]); Rw(Wg[B[pc]]+2);
                 b = a; a = W[Wg[B[pc++]]+2];         goto fetch;

   case F_s0gh:  Rb(pc+1); Rw(g+GH(pc)); Rw(Wg[GH(pc)]+0);
                 W[Wg[GH(pc)]+0] = a;        pc += 2; goto fetch;
   case F_s0g1:  Rb(pc);   Rw(g+256+B[pc]); Rw(Wg1[B[pc]]+0);
                 W[Wg1[B[pc++]]+0] = a;               goto fetch;
   case F_s0g:   Rb(pc);   Rw(g+B[pc]); Rw(Wg[B[pc]]+0);
                 W[Wg[B[pc++]]+0] = a;                goto fetch;

   case F_stp0+5: Rw(p+5);  Rw(a+Wp[5]); W[a+Wp[5]] = b; goto fetch;
   case F_stp0+4: Rw(p+4);  Rw(a+Wp[4]); W[a+Wp[4]] = b; goto fetch;
   case F_stp0+3: Rw(p+3);  Rw(a+Wp[3]); W[a+Wp[3]] = b; goto fetch;

   case F_st0p0+4: Rw(p+4); Rw(Wp[4]+0); W[Wp[4]+0] = a; goto fetch;
   case F_st0p0+3: Rw(p+3); Rw(Wp[3]+0); W[Wp[3]+0] = a; goto fetch;

   case F_st1p0+4: Rw(p+4); Rw(Wp[4]+1); W[Wp[4]+1] = a; goto fetch;
   case F_st1p0+3: Rw(p+3); Rw(Wp[3]+1); W[Wp[3]+1] = a; goto fetch;
   
   case F_rvp0+7: Rw(p+7);  Rw(a+Wp[7]); a = W[a+Wp[7]]; goto fetch;
   case F_rvp0+6: Rw(p+6);  Rw(a+Wp[6]); a = W[a+Wp[6]]; goto fetch;
   case F_rvp0+5: Rw(p+5);  Rw(a+Wp[5]); a = W[a+Wp[5]]; goto fetch;
   case F_rvp0+4: Rw(p+4);  Rw(a+Wp[4]); a = W[a+Wp[4]]; goto fetch;
   case F_rvp0+3: Rw(p+3);  Rw(a+Wp[3]); a = W[a+Wp[3]]; goto fetch;
   }

//badpc:
//   res = 4;  /* pc too large or negative  */
 
ret:
   Rw(regs+0); W[regs+0]  = a;    /* Save the machine registers  */
   Rw(regs+1); W[regs+1]  = b;
   Rw(regs+2); W[regs+2]  = c;
   Rw(regs+3); W[regs+3]  = p<<B2Wsh;
   Rw(regs+4); W[regs+4]  = g<<B2Wsh;
   Rw(regs+5); W[regs+5]  = st;
   Rw(regs+6); W[regs+6]  = pc;
   Rw(regs+7); W[regs+7]  = count;
   Rw(regs+8); W[regs+8]  = mw;
   
   return res;  // Return from this invocation of interpret
}

// cintsys and cintpos both possibly call setraster.
// This function are not defined in the standard interpreter, cinterp.o,
// but is given a dummy definition in fasterp.o (when FASTyes is defined)
// and a proper definition in rasterp.o when RASTERINGyes is
// defined. FASTyes and RASTERINGyes will never both be defined at the
// same time.
#ifdef FASTyes
BCPLWORD setraster(BCPLWORD n, BCPLWORD val)
{ return 0;
}
#endif

#ifdef RASTERINGyes
static int initraster(char *filename)
{ //printf("initraster(%s), sound=%d\n", filename, sound);
  if (sound) {
    rasterfile = fopen(filename, "wb");
    if (rasterfile==0) return 0;
    printf("Bit stream file %s opened\n", filename);
    return 1;
  } else {
    fcounttrig = count;
    addp = 0;
    fcount = 0;
    //printf("initraster: file %s opened\n",filename);
    rasterfile = fopen(filename, "w");
    if (rasterfile==0) return 0;
    //printf("Raster file %s opened, count=%d scale=%d\n",
    //        filename, count, scale);
    fprintf(rasterfile, "K%ld S%ld\n", (long)count, (long)scale);
    return 1;
  }
}

static int endraster(void)
{ if (rasterfile)
    { if (sound==0) wrline();
    return fclose(rasterfile); /* Return 0 if successful */
  }
  return -1;
}

BCPLWORD setraster(BCPLWORD n, BCPLWORD val)
{ // n=0      Open the raster file
  // n=1      Set count=val
  // n=2      Set scale=val
  // n=3      Return 0 if rastering is available
  // n=4      val=1 generate a bit stream sound file
  //          val=0 generate a raster file

  char chbuf1[256];
  char chbuf2[256];
  //printf("setraster: n=%d\n", n);
  switch((int)n)
  { case 0: // Close the raster file if it was open
            // or open the raster file
            if( endraster()) {
	      // The raster file was open and is now closed.
	      return 0;
	    } else {
	      char *name = "RASTER";
              if (val) {
	        // val is the BCPL name of the raster file
		name = b2c_str(val, chbuf1);
	      }
	      //printf("Calling initraster(%s)\n", osfname(name, chbuf2));
              return initraster(osfname(name, chbuf2));
            }

    case 1: if(val>=0) count = val;
            return count;
    case 2: if(val>=0) scale = val;
            return scale;
    case 3: return 1;    /* Rastering is available */
    case 4: // Set sound = 1 for sound data
            // or  sound = 0 for raster data.
            sound = val;
            if (sound) {
              bitpos = 1; /* Initialis the bit stream variables */
              bits  = 0;
	    }
            return 0;     // Success
    case 5: return endraster(); /* Return 0 if successful */
  }
  return 0;
}

void sort(BCPLWORD p, BCPLWORD q)
{ if(p<q)
  { BCPLWORD t;
    BCPLWORD m = addr[(p+q)>>1];
    BCPLWORD i=p, j=q;
    while(1)
    { while(addr[i]<m) i++;
      /* all k in p..i-1 => addr[k]<m
      ** addr[i] >= m
      */
      while(j>i && addr[j]>=m) j--;
      /* all k in j+1 .. q => addr[k]>=m
      ** j>=i
      */
      if(i==j) break;
      /* j>i and addr[i]>m and addr[j]<m
      */
      t = addr[i];
      addr[i] = addr[j];
      addr[j] = t;
      /* j>i and addr[i]<m and addr[j]>m
      */
      i++;
    }
    sort(p, i-1);
    j = q;
    while(1)
    { while(i<=j && addr[i]==m) i++;
      while(addr[j]!=m) j--;
      if (j<i) break;
      addr[j] = addr[i];
      addr[i] = m;
      i++;
    }
    sort(i, q);
  }
}

static void wrline(void)
{ BCPLWORD i=1, k, a=0, b;
  sort(1, addp);
  while(i<=addp && addr[i]<0) i++;
  
  while(i<=addp)
  { 
    b = addr[i++]; /* addr of next black */
    k = b-a;       /* white count, possibly zero */
    fprintf(rasterfile, "W%ld", (long)k);
    a = b; /* start of next black region */
    /* find next white */
    b++;
    while (i<=addp && addr[i]<=b) b=addr[i++]+1; 
    k = b-a;
    a = b; /* start of next white region */
    fprintf(rasterfile, "B%ld", (long)k);
  }
  fprintf(rasterfile, "N\n");
  addp = 0;
  fcounttrig += count;
  if(fcount%1000000==0) printf("fcount = %ld\n", (long)fcount);
}

void rasterpoint(BCPLWORD p)
{ if (sound) {
    /* Test the fifth bit of every accessed address */
    if ((p & 0x10) > 0) bits += bitpos;
    if (bitpos>=128) {
      /* Output the current 8-bit bit pattern. */
      fputc(bits, rasterfile);
      bitpos = 1;
      bits = 0;
    } else {
      bitpos = bitpos+bitpos;
    }
  } else {
    BCPLWORD a = p/scale;
    if (addp<UPB) addr[++addp] = a;
/*  printf("%7ld %7ld\n", (long)addp, (long)a);*/
    if (fcount >= fcounttrig) wrline();
  }
}

#endif


