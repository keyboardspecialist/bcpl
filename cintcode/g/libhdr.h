// Standard BCPL header for both Cintsys and Cintpos

// Modified by Martin Richards (c) 2 Sep 2019

/*
21/07/2020
The default global vector size is now 2000 since most machines
nowdays have plenty of memory.

22/08/2019
The thread and cvp fields have been modified to allow devices.c
to store 64 bit addresses even when the BCPL word length is 32 bits.

11/02/04 MR
Added binwrch, removed packstring, unpackstring and dqpkt
21/10/02 MR
Made compatible with libhdr of the standard BCPL distribution
*/

MANIFEST {
// For implementions for which BITSPERBCPLWORD is not a reserved word,
// uncomment one of the following lines.
//BITSPERBCPLWORD = 32
//BITSPERBCPLWORD = 64

B2Wsh = 1 + BITSPERBCPLWORD/32  // =2 for 32- bit implementations
                                // =3 for 64-bit implementations
W2Bsh = B2Wsh
ON64 = BITSPERBCPLWORD=64       // Used by programs designed to run
                                // on both 32 and 64 bit systems.
}

// All that follows is the same for both 32- and 64-bit Cintcode systems.

// Globals used in the standard (single threaded) BCPL Cintcode System
GLOBAL {
glob0:               0
globsize:            0
start:               1
stop:                2
sys:                 3  //SYSLIB   MR 18/7/01
clihook:             4
muldiv:              5  //SYSLIB   changed to G:5 MR 6/5/05
changeco:            6  //SYSLIB   MR 6/5/04
currco:              7
colist:              8
rootnode:            9  // For compatibility with native BCPL
result2:            10
tempval:            11  // A memory location used by native code
                        // floating point operations.
cis:                12
cos:                13
currentdir:         14
level:              15
longjump:           16
createco:           17
deleteco:           18
callco:             19
cowait:             20
resumeco:           21
initco:             22
startco:            23
globin:             24
getvec:             25
rdargs2:            26   // MR 19/11/2014  allowing double length arg format
freevec:            27
abort:              28
sysabort:           29
packstring:         30
unpackstring:       31
getword:            32
putword:            33
randno:             34
setseed:            35
sardch:             36
sawrch:             37
rdch:               38
binrdch:            39
unrdch:             40
wrch:               41
binwrch:            42
deplete:            43
readwords:          44
writewords:         45
initio:             46
splitname:          47
findinput:          48
findoutput:         49
findinoutput:       50
findupdate:         51
findstream:         52
pathfindinput:      53
getremipaddr:       54
settimeout:         55
selectinput:        56
selectoutput:       57
input:              58
output:             59
endread:            60
endwrite:           61
endstream:          62
note:               63
point:              64
rewindstream:       65
appendstream:       66
stepstream:         67
setrecordlength:    68
recordpoint:        69
recordnote:         70
get_record:         71
put_record:         72
writeflt:           73  // Added Feb 2018
readflt:            74  // Added Feb 2018
copyobj:            75
deletefile:         76
renamefile:         77
freeobj:            78
copydir:            79
locatedir:          80  // Possibly in cintpos
locateobj:          81  // Possibly in cintpos
createdir:          82  // Possibly in cintpos
readn:              83
newline:            84
writed:             85
writen:             86
writehex:           87
writeoct:           88
writes:             89
writet:             90
writeu:             91
writez:             92
get_textblib:       93  //BLIB version
get_text:           93  //BLIB overridden version
writef:             94  //BLIB
sawritef:           95
capitalch:          96
compch:             97
compstring:         98
copystring:         99
string_to_number:  100
str2numb:          101
rdargs:            102
rditem:            103
findarg:           104
loadseg:           105
unloadseg:         106
callseg:           107
datstring:         108
datstamp:          109
dat_to_strings:    110
string_to_dat:     111
setbit:            112
testbit:           113
copy_words:        114
clear_words:       115
copy_bytes:        116
setlogname:        117
getlogname:        118
intflag:           119
newpage:           120
instrcount:        121
setbulk:           122
stackfree:         123  // Returns the number of free stack locations
settimeoutact:     124
deleteself:        125
codewrch:          126  // Write an extended character in UTF8 or GB2312 format
randseed:          127
delay:             128  // delay(msecs)
delayuntil:        129  // delayuntil(days, msecs)
findappend:        130  // Added 18/01/11
memoryfree:        131  // Returns the amount of free and used memory.

//##### CLI uses globals 132 - 149 #####

cli_tallyflag:     132
cli_init:          133  // Not used
cli_result2:       134
cli_data:          135  // CLI dependent data  MR 10/7/03
cli_commanddir:    136
cli_returncode:    137  // This holds the returncode of the most
                        // recently executed command. It can be
			// inspected by commands such as if and why,
			// and may also be passed back to the
			// enclosing operating system shell.
cli_commandname:   138
cli_faillevel:     139
cli_prompt:        140
cli_standardinput: 141
cli_currentinput:  142
cli_commandfile:   143  // Name of temporary command file used in
                        // command-commands
cli_status:        144  // Contains the CLI status flags such as
                        // clibit_noprompt, clibit_comcom or clibit_maincli

cli_preloadlist:   145
cli_currentoutput: 146  // Not used
cli_defaultstack:  147  // The coroutine stack size of the next cli command.
cli_standardoutput:148
cli_module:        149  // The code segment of the currently executing
                        // command, if any.

//##### Cintpos uses globals 150 - 179 #####

srchwk:            150
tcb:               151
taskid:            152
createtask:        153
deletetask:        154
changepri:         155
setflags:          156
testflags:         157
hold:              158
unhold:            159;  release: 159
taskwait:          160
qpkt:              161
endtask:           162

sendpkt:           165 // Overridden when in multievent mode

returnpkt:         169

consoletask:       171
createdev:         172
deletedev:         173

set_process_name:  175

peercom:           179

// More globals used by both cintsys and cintpos

writee:            180 // Write a floating point number in exponential form
setvec:            181 // (v, n, a1,..,a16) Copy values into v
fault:             182
sxpushval:         183 // (sxv, val)  Push val into a self expanding vector
cmpdats:           184 // (dat1, dat2) compare date and times, added 14/08/2023

// Globals 190-199 are variables not reset between CLI commands
current_language:  190 // Potentially used by get_text when converting
                       // error numbers to text.
}

MANIFEST {

tg = 190   // First user global not reset between CLI commands
ug = 200   // First user global

bytesperword    = 1<<B2Wsh
bitsperbyte	= 8
bitsperword	= bitsperbyte * bytesperword
mcaddrinc	= bytesperword
minint          = 1<<(bitsperword-1)  // = #x80....0
maxint          = minint - 1          // = #x7F....F

endstreamch	= -1  // ch returned at EOF
timeoutch	= -2  // ch returned when none available before timeout
pollingch	= -3  // ch returned when none available when polling

// Object module format types

t_hunk    = 1000 // A hunk in ASCII hex
t_reloc   = 1001
t_end     = 1002
t_hunk64  = 2000 // A hunk in ASCII hex for 64-bit Cintcode
t_reloc64 = 2001
t_end64   = 2002
t_bhunk   = 3000 // A hunk in binary
t_bhunk64 = 4000 // A hunk in binary for 64-bit Cintcode

globword  = #xFFFFFFFF8F8F0000  // MR 7/9/2006 (for 32 and 64-bit versions)
stackword = #xFFFFFFFFABCD1234
deadcode  = #xFFFFFFFFDEADC0DE
sectword  = #x000000000000FDDF
entryword = #x000000000000DFDF

// Important global variable numbers
g_globsize =  0
g_sys      =  3
g_currco   =  7
g_colist   =  8
g_rootnode =  9
g_result2  = 10

g_memsize  = 14
g_keyboard = 20
g_screen   = 21

// co-routine stackbase offsets

co_pptr = 0
co_parent
co_list
co_fn
co_size
co_c

InitObj  = 0  // Initialisation and closing methods for objects
CloseObj = 1

// Rootnode manifests

rootnodeaddr = 100  // MR 21/10/02 for compatibility with Cintpos
                    // Not used in native code versions

rtn_tasktab = 0
rtn_devtab
rtn_tcblist
rtn_crntask
rtn_blklist
rtn_tallyv         // 5

rtn_clkintson
rtn_lastch         // For sadebug polling input
rtn_insadebug      // Looked at by ttyin device

rtn_bptaddr        // Breakpoint addresses      ) MR 20/9/02
rtn_bptinstr       // Breakpoint instructions   )
rtn_dbgvars        // The Standalone Debug variables

rtn_clwkq          // 12
rtn_membase
rtn_memsize
rtn_info
rtn_sys
rtn_boot           // BOOT code
rtn_klib           // KLIB code segments
rtn_blib           // BLIB code segments
rtn_keyboard       // Keyboard stream
rtn_screen         // Screen stream

rtn_vecstatsv      // 22
rtn_vecstatsvupb   // 23

rtn_intflag        // Set to TRUE by ctrl-c and FALSE by sadebug
rtn_dumpflag       // =TRUE for memory dump to DUMP.mem
rtn_envlist        // List of logical name-value pairs
                   // used by setlogname and getlogname
rtn_abortcode      // Latest reason for leaving the interpreter
rtn_context        // Context of DUMP.mem
                   // 1 dump caused by second SIGINT
                   // 2 dump caused by SIGSEGV
                   // 3 fault in BOOT or standalone debug
                   // 4 dump by user calling sys(Sys_quit, -2)
                   // 5 dump caused by non zero user fault code
                   // 6 dump requestested from standalone debug

rtn_sysp           // Latest setting of p pointer at SIGINT or SIGSEGV
rtn_sysg           // Latest setting of p pointer at SIGINT or SIGSEGV
rtn_sysst          // Latest setting of st
                   // st = 0    in a user task, interrupts enabled
                   // st = 1    in BOOT,        interrupts disabled
                   // st = 2    in KLIB,        interrupts disabled
                   // st = 3    in the ISR      interrupts disabled

rtn_idletcb        // The IDLE TCB (for debugging)
rtn_adjclock       // Real time clock adjustment in minutes

rtn_dcountv  // 34 // the Debug Counts vectors

// The following four variables are set by boot.b
// and used by programs such as cli.b, bcpl.b and c.b

rtn_rootvar        // The environment variable giving the
                   // system root directory, eg "BCPLROOT" or "POSROOT"
rtn_pathvar        // The environment variable giving the directories
                   // searched by loadseg, eg "BCPLPATH" or "POSPATH"
rtn_hdrsvar        // The environment variable giving the directories
                   // containing BCPL headers, eg "BCPLHDRS" or "POSHDRS"
rtn_scriptsvar     // The environment variable giving the directories
                   // containing cli scripts, eg "BCPLSCRIPTS" or "POSSCRIPTS"
rtn_boottrace      // =0, 1, 2 or 3 as set by -v and -vv options to
                   // trace the progress of booting the system.

rtn_days           // Days since 1 Jan 1970 (1978 old dat format)
rtn_msecs          // Milliseconds since midnight
rtn_usecs          // Not used anymore but hold -12345 (14/08/2023)
                   // This may one day be microsecs since last millisec
		   // Alternatively possibly [days,secs,usecs]

rtn_mc0            // Machine address of the start of the
                   // Cintcode memory.
rtn_mc1            // Other values used by the MC package.
rtn_mc2          
rtn_mc3          

rtn_system         // =1 for cintsys, =2 for cintpos, =0 otherwise

rtn_icountmax      // Estimate of the number of Cintccode instructios
                   // executed per milli-second.

rtn_joystickfd     // The joystick fd
rtn_joystickfd1    // The joystick fd second word
rtn_joybuttoncount // The number of joystick buttons
rtn_joyaxiscount   // The number of joystick axes
rtn_joycurrbuttons // The bit pattern of currently pressed buttons
rtn_joybuttons     // The bit pattern of recently pressed buttons
rtn_joyaxis0       // The value of axis0
rtn_joyaxis1       // The value of axis1
rtn_joyaxis2       // The value of axis2
rtn_joyaxis3       // The value of axis3
rtn_joyaxis4       // The value of axis4
rtn_joyaxis5       // The value of axis5
rtn_joyaxis6       // The value of axis6

rtn_hostaddsize    // Size in bits of a machine address on the
                   // host machine

rtn_gvecsize       // The upper bound of global vectors from now on.
                   // The default is now 2000.
		   // It can be set by the -g option in cinsys
		   // or by a user progtam.
		   // Added 28/12/2019

// The following are for the new implementatin of tty input.
// boot allocates ttyinbuf withupb=255
rtn_ttyinwkq       // List of outstanding ttyin pkts
rtn_ttyinwkqe      // Zero or the last ttyin pkt in the list

rtn_quietflag      // set by the cintsys -q option

rtn_upb = 80       // Leave some unused entries

// SYS functions
Sys_setcount        =  -1
Sys_quit            =   0
Sys_rti             =   1
Sys_saveregs        =   2
Sys_setst           =   3
Sys_tracing         =   4
Sys_watch           =   5
Sys_tally           =   6
Sys_interpret       =   7

Sys_sardch          =  10
Sys_sawrch          =  11
Sys_read            =  12
Sys_write           =  13
Sys_openread        =  14
Sys_openwrite       =  15
Sys_close           =  16
Sys_deletefile      =  17
Sys_renamefile      =  18
Sys_openappend      =  19

Sys_getvec          =  21
Sys_freevec         =  22
Sys_loadseg         =  23
Sys_globin          =  24
Sys_unloadseg       =  25
Sys_muldiv          =  26
Sys_intflag         =  28
Sys_setraster       =  29
Sys_cputime         =  30
Sys_filemodtime     =  31
Sys_setprefix       =  32
Sys_getprefix       =  33
Sys_graphics        =  34  // Not implemented

Sys_seek            =  38  // MR 10/11/01
Sys_tell            =  39  // MR 10/11/01
Sys_waitirq         =  40  // MR  4/02/02
Sys_lockirq         =  41  // MR 24/02/03
Sys_unlockirq       =  42  // MR 24/02/03
Sys_devcom          =  43  // MR  4/02/02
Sys_datstamp        =  44  // MR 29/03/10

Sys_filesize        =  46  // MR 15/03/02
Sys_openreadwrite   =  47  // MR 19/03/02
Sys_getsysval       =  48  // MR 18/11/02
Sys_putsysval       =  49  // MR 18/11/02
Sys_shellcom        =  50  // MR 13/01/03
Sys_getpid          =  51  // MR  7/10/03
Sys_dumpmem         =  52  // MR 29/10/03 Used only in BOOT.b
Sys_callnative      =  53  // MR 24/04/04
Sys_platform        =  54  // MR 06/04/05 architecture and OS
Sys_inc             =  55  // MR 17/12/04
Sys_buttons         =  56  // MR 21/06/06 Button on the GP2X
Sys_delay           =  57  // MR 01/05/10 Delay until a specified date and time
Sys_sound           =  58  // MR 11/09/07 Sound functions
Sys_callc           =  59  // MR 28/01/09 Call the C function
                           //             callc(args,g)
Sys_trpush          =  60  // MR 05/02/10 Push a trace value
Sys_settrcount      =  61  // MR 05/02/10 Set trcount
Sys_gettrval        =  62  // MR 05/02/10 Get a pushed trace value
Sys_flt             =  63  // MR 21/07/10 Floating point operations
Sys_pollsardch      =  64  // MR 07/03/11 Return next ch or -3
Sys_incdcount       =  65  // MR 06/03/12 Increment a specified debug counter.

Sys_sdl             =  66  // MR 30/05/12 SDL features
Sys_gl              =  67  // MR 12/01/14 OpenGL features
Sys_ext             =  68  // MR 14/04/14 EXT user extension features
Sys_joy             =  69  // MR 22/01/18 Joystick features
Sys_settracing      =  70  // MR 26/02/20 Set settracing
Sys_getbuildno      =  71  // MR 03/02/21 Get the current build number
                           //    such as bld_linux or bld_RaspiSDL.
                           // It also returns the build flags in result2.
Sys_alsa            =  72  // MR 06/06/23 Alsa features
Sys_memmovewords    =  73  // MR 14/06/23 (dest, src, n) dest and src are
                           //    word addresses within the Cintcode memory.
			   //    The dest and src regions may overlap.
Sys_memmovebytes    =  74  // MR 14/06/23 (dest, src, n) dest and src are
                           //    byte addresses within the Cintcode memory.
			   //    The dest and src regions may overlap.

bootregs = 11 // Registers used to enter the function start in boot.b
cliregs  = 21 // Registers used by BOOT to start the CLI
klibregs = 21 // Registers used by BOOT to start KLIB
saveregs = 31 // Registers of an interrupt enabled user program at
              // the time a Cintpos interrupt was entered. These registers
              // are only valid when the interrupt service routine is
              // active. In this state register st=3.
isrregs  = 41 // Registers for the Cintpos interrupt service routine

// Build numbers, more will be added later.
// Their values mus agree with those in cintsys.h
// Build numbers are returned by sys(Sys_getbuildno)
bld_unknown               =  0
bld_linux                 =  1
bld_linuxSDL              =  2
bld_linuxSDL2             =  3
bld_linuxGL               =  4
bld_linuxSDLGL            =  5
bld_linuxSDL2GL           =  6
bld_linuxiSH              =  7 // Alpine linux App for iPad and iPhone

bld_Raspi                 = 21
bld_RaspiSDL              = 22
bld_RaspiSDL2             = 23
bld_RaspiSDLGL            = 24
bld_RaspiSDL2GL           = 25

bld_MacOSX                = 31
bld_MacOSXSDL             = 32
bld_MacOSXSDL2            = 33
bld_MacOSXSDLGL           = 34
bld_MacOSXSDL2GL          = 35

bld_VmsVax                = 41
bld_Win32                 = 42
bld_CYGWIN                = 43


//Build flags
bldf_sound     =  1
bldf_callc     =  1<<1
bldf_joystick  =  1<<2
bldf_ALSAavail =  1<<3
bldf_SDLavail  =  1<<4
bldf_GLavail   =  1<<5


id_inscb	= #x81  // MR 21/10/02
id_outscb	= #x82  // MR 21/10/02
id_inoutscb	= #x83  // MR 21/10/02
id_appendscb	= #x84  // MR 18/01/11

scbt_net     =  2  // Non interactive TCP/IP stream
scbt_file    =  1  // Non interactive disc file stream
scbt_ram     =  0  // Non interactive RAM stream
scbt_console = -1  // Interactive -- output triggered by '*n' etc
scbt_mbx     = -2  // Interactive MBX stream
scbt_tcp     = -3  // Interactive TCP/IP stream

scb_maxnamelen = 31

scb_id = 0         // id_inscb, id_outscb or id_inoutscb
scb_type           // <=0 interactive stream, >0 block file
scb_task           // 0 or the task associated with this stream
scb_buf            // 0 or the byte buffer for this stream
scb_pos            // position of next character to be transferred
scb_end            // number of valid bytes in the buffer or -1
scb_rdfn           // zero or function to replenish the buffer
scb_wrfn           // zero or function to deplete the buffer
scb_endfn          // zero or function to close down the stream
scb_block          // Current block number of a disc file
scb_write          // Buf updated to but not yet written to disc
scb_bufend         // Size of buf in bytes
scb_lblock         // Number of last block of a disc file
scb_ldata          // Bytes in last block of a disc file
scb_blength        // Length of a disc block in bytes (typically 4096)
scb_reclen         // Record length in bytes for some files
scb_fd             // File or mailbox descriptor MR 18/4/02
scb_fd1            // File or mailbox descriptor, second word
scb_timeout        // The stream timeout value in milli-seconds MR 26/3/02
                   // = 0  means no time out is to be applied
                   // =-1  only transfer data that is immediately possible
scb_timeoutact     // Action if a timeout occurs
                   // = 0  Try the operation again
                   // =-1  Abort the operation
                   // =-2  Return timeoutch
scb_encoding       // Unicode encoding: UTF8 (=-1) or GB2312 (=-2),
                   // used by uniwrch.
scb_name           // Pointer to name of stream, see below

scb_nameeend = scb_name + scb_maxnamelen/bytesperword
                   // Last word of space for name

scb_size
scb_upb = scb_size-1

// Floating point operations used in sys(Sys_flt, op,...)
// 32- or 64-bit floating point will be used depending on
// whether 32- or 64-bit Cintcode is being used.
// Any change to these definitions require a corresponding
// change in cinterp.c and rasterp.c
fl_avail=0
fl_mk; fl_unmk
fl_float; fl_fix; fl_abs
fl_mul; fl_div; fl_mod
fl_add; fl_sub; fl_pos; fl_neg
fl_eq; fl_ne; fl_ls; fl_gr; fl_le; fl_ge

fl_acos=20
fl_asin
fl_atan
fl_atan2
fl_cos
fl_sin
fl_tan
fl_cosh
fl_sinh
fl_tanh
fl_exp     //=30
fl_frexp
fl_ldexp
fl_log
fl_log10

fl_pow=36  //=36
fl_sqrt
fl_ceil
fl_floor
fl_modf    //=40   Invoke the C function modf to extract the
           //      fractional and integer parts of a floating
           //      point number

fl_N2F     // =41 Convert scaled fixed point to floating
fl_F2N     //     Convert floating point to scaled fixed point
fl_radius2 //     Return the distance from the origin to point(x,y)
fl_radius3 //     Return the distance from the origin to point(x,y,z)
fl_64to32  // =45 Convert from 64 to 32 bit floating point. Only
           //     used when running on 64 bit BCPL. Needed for the
	   //     OpenGL interface.
fl_32to64  // =46 Convert from 32 to 64 bit floating point. Only
           //     used when needing to generate a 64 bit floating
	   //     point while running on 32 bit BCPL. The result
	   //     holds the LS 32 bits and result2 the MS bits.

// Unicode encodings
UTF8 = -1
GB2312 = -2

return_severe    =  20
return_hard      =  10
return_soft      =   5
return_ok        =   0
cli_module_gn    =  149
cli_initialstack =  50000       // Changed 21/5/2001
cli_initialfaillevel = return_hard

// cli_status flags
clibit_noprompt  =  #b000000001  // Don't output a prompt
clibit_eofdel    =  #b000000010  // Delete this task if EOF received
clibit_comcom    =  #b000000100  // Currently execution a command-command
clibit_maincli   =  #b000001000  // Executing the main CLI
clibit_newcli    =  #b000010000  // Executing a new CLI
clibit_runcli    =  #b000100000  // Executing a CLI invoked by run
clibit_mbxcli    =  #b001000000  // Executing an MBX CLI
clibit_tcpcli    =  #b010000000  // Execution a TCP CLI
clibit_endcli    =  #b100000000  // endcli has been executed on this CLI


notinuse	= -1

// standard packet offsets

pkt_link =  0
pkt_id   =  1; pkt_devid =  1; pkt_devtaskid = 1; pkt_taskid = 1
pkt_type =  2; pkt_op    =  2
pkt_res1 =  3; pkt_r1    =  3
pkt_res2 =  4; pkt_r2    =  4
pkt_arg1 =  5; pkt_a1    =  5
pkt_arg2 =  6; pkt_a2    =  6
pkt_arg3 =  7; pkt_a3    =  7
pkt_arg4 =  8; pkt_a4    =  8
pkt_arg5 =  9; pkt_a5    =  9
pkt_arg6 = 10; pkt_a6    = 10

// TCB offsets

tcb_link	=  0
tcb_taskid	=  1
tcb_pri		=  2
tcb_wkq		=  3
tcb_state	=  4
tcb_flags	=  5
tcb_stsiz	=  6
tcb_seglist	=  7
tcb_gbase	=  8
tcb_sbase	=  9
tcb_active	= 10 // TRUE if the task is fully activated

tcb_regs        = 11
tcb_a           = tcb_regs
tcb_b           = 12
tcb_c           = 13
tcb_p           = 14
tcb_g           = 15
tcb_st          = 16
tcb_pc          = 17
tcb_count       = 18

tcb_namebase    = 19 // Space for upto 15 chars of task name

tcb_upb = tcb_namebase + 15/bytesperword + 1

// The DCB structure -- Only used by Cintpos
Dcb_type    =  0   // Device type: clk, ttyin, ttyout, fileop, tcpdev, etc
Dcb_devid   =  1   // The device id (<0)
Dcb_wkq     =  2   // The device work queue
Dcb_op      =  3   // op  set by devcommand
Dcb_arg     =  4   // arg set by devcommand
Dcb_irq     =  5   // TRUE if the device has a packet to return
Dcb_intson  =  6   // TRUE if the device may generate interrupts
Dcb_flag    =  7   // =1 if the device has requested an interrupt
                   // =0 after the interrupt request has been removed from
                   // the fifo and the corresponding pkt dequeued from the
                   // wkq. This field is protected by irq_mutex
Dcb_var0    =  8   // Variables (currently not) used by some devices
Dcb_var1    =  9
Dcb_var2    = 10
Dcb_var3    = 11
Dcb_var4    = 12

Dcb_threadp = 14   // M/C address of location holding the thread id
Dcb_cvp     = 16   // M/C address of its condition variable. It is
                   // signalled when the wkq has another packet for
                   // the device to process and flag is set to zero.
                   // This is used with irq_mutex 
Dcb_upb     = 17

// Device types
Devt_clk     = 1
Devt_ttyin   = 2
Devt_ttyout  = 3
Devt_fileop  = 4
Devt_tcpdev  = 5

// Device commands
Devc_create    = 1
Devc_destroy   = 2
Devc_start     = 3
Devc_stop      = 4
Devc_setintson = 5

// Standard task numbers in Cintpos
Task_cli            =     1
Task_debug          =     2
Task_consolehandler =     3
Task_filehandler    =     4
Task_mbxhandler     =     5
Task_tcphandler     =     6

// Scheduling states and flags
State_pkt           =     #b0001
State_hold          =     #b0010
State_wait          =     #b0100
State_int           =     #b1000
State_dead          =     #b1100

flag_a       = 1<<0                // MR 05/05/10
flag_b       = 1<<1                // Note that the bit
flag_c       = 1<<2                // positions have changed
flag_d       = 1<<3
flag_e       = 1<<4

// Assignment vectors
Ass_link  = 0
Ass_task  = 1
Ass_dir   = 2
Ass_type  = 3
Ass_dev   = 4
Ass_name  = 5

g_grbase    = 450 // Number of the first global in the Graphics library
g_bdrawbase = 450 // Number of the first global in the bdrawlib library
g_sdlbase   = 450 // Number of the first global in the SDL library
g_glbase    = 450 // Number of the first global in the GL library
g_sndbase   = 450 // Number of the first global in the Sound library
g_alsabase  = 500 // Number of the first global in the ALSA library
g_extbase   = 950 // Number of the first global in the EXT library
}

