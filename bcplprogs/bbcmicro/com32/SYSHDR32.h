
// Copyright RICHARDS COMPUTER PRODUCTS (C) 1982
// SYSHDR32

// Modified to run under 32-bit Cintcode

GLOBAL $(
MAXGLOB:0;ABORTCODE:704;ABORTLABEL:705;ABORTLEVEL:706;STOREFILES:716;STREAMCHAIN:717
SYSINDEX:718;TRAPSTACK:719;MAINSTACK:720;LASTERROR:721;CURRCO:722;ENDPROG:724
STACKSIZE:730;CHANGECO:732;CLIINSTR:733;ENDTRAP:734;FSTYPE:739;WRITET:740;WRITEU:741;HEAP:748
HEAPEND:754;ERRORSTREAM:758;FILENAME:759;STCNTRL:761;COMMON2:762;COMMON3:763;DELXFILE:765;FINDXINPUT:768
FINDXOUTPUT:769;WRITEA:786;WRITEBA:787;WRITEDB:789;WRITEOCT:793;EXTSFILE:802;TRUNCVEC:803
UNLOADSEG:812;SFCNTRL:813;LINKEDFILES:814;VDUINFO:816;LIBBASE:817;CLOSESTREAM:820;TRAPSTART:822
SFSTATE:824;APTOVEC:827;FINDSTFILE:948;CONTPRG:849;COLIST:866;ERRORMSG:902
SHUFFLE:903;FAULTROUTINE:905
TIDYSTATE:908;RDTOBLOCK:909;MOVEBYTE:913;BACKMVBY:918;BACKMOVE:919
$)
MANIFEST $(
BITSPERWORD=16;BYTESPERWORD=2;
MAXINT=32767;MININT=-32767
I.LIBBASE=0;I.INTBASE=1;I.TRST=2;I.FLAGS=3
I.LIMIT=4;I.CSTATE=5;I.TSTATE=6;I.RSTATE=7
I.TIME=8;I.JADD=9;I.TOD=10;I.DATE=11
I.DEFSPACE=12;I.RESTART=13
R.MCST=0;R.CURRCO=1;R.SP=2;R.PC=3;R.A=4;R.B=5;R.C=6
M.TIMED=1;M.TRAPESC=2;M.TRAPGV=4;M.TRAPFLAGS=#X80;M.TRAPPED=#X100
F.INST=0;F.JUMP=1;F.CALL=2;F.COUNT=3;F.LASTTRAP=4;F.JPTIME=5
J.FROM=0;J.TO=1
ENTRYWORD=#XDFDF
SECTWORD=#XFDDF;NEEDSWORD=#XFEED
T.END=992;T.HUNK=1000;T.MC=1001;T.RELOC=1002
S.TYPE=4
SF.BSIZE=10
DV.S=7;DV.F=8
$)

