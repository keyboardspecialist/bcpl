/*
This progam is designed to test the BCPL ALSA facilities
including wav_input, wave_output, midi_input and midi_output.
*/

// Typically load the BCPL ALSA library as a separate section.

GET "libhdr"

MANIFEST { s_alsabase=300 }

GET "alsa.h"
GET "alsa.b"

.

GET "libhdr"

MANIFEST { s_alsabase=300 }

GET "alsa.h"

/*
This only runs on cintsys if it was build with the ALSA asound library.
This can be installed by the following shell command.

  sudo apt-get install libasound2-dev

Currently a suitable cintsys can be built by

make -f Makefile-alsa clean
make -f Makefile-alsa

*/

LET start() = VALOF
{ LET str = VEC 64 // Allow string of at least 256 bytes.
                   // It is used when converting C strings
		   // to BCPL strings.
  FOR i = 0 TO 64 DO str!i := 0
  //writef("*nalsatst entered*n")
  UNLESS sys(Sys_alsa, alsa_avail) DO
  { writef("The ALSA library is not available*n")
    RESULTIS 0
  }
  //writef("The ALSA library is available*n*n")

  //sys(Sys_alsa, alsa_setscheduler) // Does not work on my version of Linux

IF FALSE DO
  FOR n = 0 TO 1 TEST sys(Sys_alsa, alsa_longname, n, str)
  THEN { writef("*nCard %n has name: %s*n", n, str)
         //abort(1000)
       }
  ELSE { writef("*nCard %n does not exist*n", n)
         //abort(1000)
       }

  //abort(2345)
  testmicrophone()
   
  writef("*nend of test*n")
  RESULTIS 0
}

AND testmicrophone() BE
{ //writef("testmicrophone: entered*n")
  testmicrophone1()
  //abort(1234)
}

AND testmicrophone1() BE
{ LET micCB = VEC 20
  LET name         = "hw:1"    // USB microphone
//LET name         = "hw:0,1"
  LET channels     = 1         // 1=Mono 2=Stereo
  LET rate         = 44100     // Rate 44100 frames per sec
  LET periodframes = 45
  
  LET periodbytes = periodframes * channels * 2 // Number of bytes per period
                                                // assuming 16-bit samples.
  LET periodwords = (periodbytes>>B2Wsh)+1 // Safe size in words of a buffer
                                           // to hold one periods
  LET periodbuf   = getvec(periodwords-1)  // BCPL vector for one period of H/W frames

  LET bufupb = 44100*10*channels   // upb of buf for 10 seconds of mono
                                   // or stereo frames
  LET buf = getvec(bufupb)         // Buffer for the mono or stereo frames
  AND bufp = ?                     // Number of frames in buf
  LET days, msecs, dummy = 0, 0, 0 // For the date stamp of the most
                                   // recent sample
                                    
  AND res = 0
  
  writef("*nTesting microphone: %s*n", name)
  //abort(1000)
  
  FOR i = 0 TO 20     DO micCB!i := 100+i
  FOR i = 0 TO bufupb DO buf!i   := (i+10) MOD 100
  bufp  := 0
  buf!0 := bufp // buf is initially empty
  
  // Initialise the wav_input control block for the microphone
  
  micCB!0 := channels     // channels is 1 or 2, mono or stereo
  micCB!1 := rate         // rate samples or pairs per second
  micCB!2 := periodframes // Number of frames in a period
  micCB!3 := periodbytes  // Size of a period in bytes
  micCB!4 := periodwords  // Size of aperiod in words
  micCB!5 := periodbuf    // The BCPL vector to hold a period

  // micCB is passed as an argument when reading microphone samples
  // or closing the microphone stream.
  
  writef("micCB: channels=%n rate=%n *
         *periodframes=%n periodbytes=%n periodwords=%n periodbuf=%n*n",
         channels, rate, periodframes, periodbytes, periodwords, periodbuf)

  writef("alsatst: Calling sys(Sys_alsa, alsa_open_wav_input, *"%s*", *
	 *micCB=%n)*n*n", name, micCB)
  
  //Open the microphone stream
  UNLESS sys(Sys_alsa, alsa_open_wav_input, name, micCB) DO
  { writef("Failed to open the microphone stream %s*n", name)
    RETURN
  }
  writef("The microphone stream %s was successfully opened*n", name)

  writef("*nalsatst: About to call: sys(Sys_alsa, alsa_close_wav_input, micCB)*n")
  //sys(Sys_alsa, alsa_close_wav_input, micCB)
//abort(5678)

  // Read all microphone samples currently available into buf.
  // bufp is the position in buf of the next sample to read. When
  // buf is nearly full the samples are moved down throwing away
  // the older samples.
  // Suppose bufupb=800 and bufp=780
  // Let midpt = bufupb/4 = 400
  // Let n be the number of elements in buf not currently holding
  // samples. This will be bufupb+1-bufp=21.
  // In n < bufupb/8=100 the buffer is nearly full and
  // so the samples in buf will be moved down to make the buffer
  // half full.
  // The sample at position bufp-1=779 will thus be moved to
  // position midpt-1=399, a distance of 779-399=380
  // The sample at position 379(= bufp-midpt-1) will be moved
  // to position zero.
  // Number of samples moved is bufp-midpt = 780-400 = 380
  
  { LET n = bufupb-bufp+1 // The number of unused element in buf
                          // eg 21
    LET midpt = bufupb/2  // eg 400
    IF n < bufupb/8 DO
    { // The buffer is nearly full so move the samples down
      // to make it half full.
      LET k = bufp-midpt // The distance each sample will move
                         // eg from bufp-1 to midpt-1
		 	 // eg from 379    to 399
      // All sample positions from 0 to midpt-1 will be updated
      // so the number of samples to move is midpt
      sys(Sys_memmovewords, buf+k-1, buf, midpt)
      bufp := midpt
      // The samples are now in buf!0 to buf!(bufp-1)
      // and bufp=midpt
    }

    writef("alsatst: Calling: sys(Sys_alsa, alsa_wav_read, micCB, buf+bufp, bufupb-bufp)*n")
    res := sys(Sys_alsa, alsa_wav_read, micCB, buf+bufp, bufupb-bufp)
    // res is negative or the number of frames read
    abort(3456)
    // Set date stamp of the latest sample
    IF res>0 DO
      datstamp(@days) // The time of the latest saple transferred, should be done by alsa_wav_read
      
    IF res<0 DO
    { writef("There was a problem reading microphone samples*n")
      abort(999)
    }
    // res>0 ie more samples have been read
    bufp := bufp+res
  } REPEATWHILE bufp>bufupb // Possible more samples
  
  // The most recent samples are now in buf!0 to buf!bufp
  
  writef("buf=%n bufp=%n*n", buf, bufp)
  FOR i = 0 TO 256 DO
  { IF i MOD 16 = 0 DO writef("*n%i5: ", i)
    writef(" %i5", buf!i)
  }
  newline()

  writef("*nalsatst: About to call: sys(Sys_alsa, alsa_close_wav_input, micCB)*n")
  //sys(Sys_alsa, alsa_close_wav_input, micCB)
  abort(3456)
}

