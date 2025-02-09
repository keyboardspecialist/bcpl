/*
This program outputs some information about the current SDL interface.

Implemented by Martin Richards (c) February 2013

13/09/2019
Modified to work in 32 and 64 bit BCPL on 32 and 54 bit machines.

*/

GET "libhdr"
GET "sdl.h"
GET "sdl.b"          // Insert the library source code
.
GET "libhdr"
GET "sdl.h"

GLOBAL {
  done:ug
}

LET plotscreen() BE
{ LET maxy = screenysize-1
  // Surface info structure
  LET flags,
      fmt, fmt1,
      w, h, pitch,
      pixels, pixels1,
      cliprectx, cliprecty, cliprectw, cliprecth, 
      refcount =
          0,
	  0, 0,
	  0, 0, 0,
	  0, 0,
	  0, 0, 0, 0,
          0

LET aa,bb,cc,dd = 1,2,3,4

// Format info structure
  LET palette, palette1, bitsperpixel, bytesperpixel,
      Rmask, Gmask, Bmask, Amask,
      Rshift, Gshift, Bshift, Ashift,
      Rloss, Gloss, Bloss, Aloss,
      colorkey, alpha = 0,0,0,0,
                        0,0,0,0,
			0,0,0,0,
			0,0,0,0,
			0,0

  // Video info structure
  LET videoflags, blit_fill, video_mem, videoformat, bpp = 0,0,0,0, 0

  fillsurf(maprgb(120,120,120))

  setcolour(maprgb(255,255,255))

  sys(Sys_sdl, sdl_getsurfaceinfo, @screen, @flags)
  sys(Sys_sdl, sdl_getfmtinfo, @format, @palette)
  sys(Sys_sdl, sdl_videoinfo, @videoflags)

  // Screen surface info
  drawf(20, maxy- 20, "Screen Surface Info")

  drawf(30, maxy- 40,
        "flags=%8x w=%n h=%n pitch=%n",
         flags,    w,   h,   pitch)

  drawf(30, maxy- 60,
        "Clip rect = [%n, %n, %n, %n]",
         cliprectx, cliprecty, cliprectw, cliprecth)

  // Screen format info
  drawf(20, maxy- 80, "Screen Format Info")
  drawf(30, maxy-100,
        "palette=%n bitsperpixel=%n bytesperpixel=%n",
         palette,   bitsperpixel,   bytesperpixel)
  drawf(30, maxy-120,
        "Rmask=%8x Gmask=%8x Bmask=%8x Amask=%8x",
         Rmask,    Gmask,    Bmask,    Amask)
  drawf(30, maxy-140,
        "Rshift=%n Gshift=%n Bshift=%n Ashift=%n",
         Rshift,   Gshift,   Bshift,   Ashift)
  drawf(30, maxy-160,
        "Rloss=%n Gloss=%n Bloss=%n Aloss=%n",
         Rloss,   Gloss,   Bloss,   Aloss)
  drawf(30, maxy-180,
        "colorkey=%8x alpha=%n",
         colorkey,    alpha)

  // Video info
  drawf(20, maxy-220, "Video Info")
  drawf(30, maxy-240,
        "videoflags=%8x blit_fill=%8x video_mem=%n bpp=%n",
         videoflags,    blit_fill,    video_mem, bpp)
//IF FALSE DO
  { LET n = sys(Sys_sdl, sdl_numjoysticks)
    drawf(20, maxy-280, "Number of joysticks  %2i", n)
    FOR j = 0 TO n-1 DO
    { LET ok = sys(Sys_sdl, sdl_joystickopen, j, @joystick)
      LET axes = sys(Sys_sdl, sdl_joysticknumaxes, @joystick)
      LET buttons = sys(Sys_sdl, sdl_joysticknumbuttons, @joystick)
      LET hats = sys(Sys_sdl, sdl_joysticknumhats, @joystick)
      drawf(20, maxy-300-80*j, "Joystick %n", j+1)
      drawf(30, maxy-320-80*j,
           "Number of axes      %2i", axes)
      FOR a = 0 TO axes-1 DO
        drawf(250+60*a, maxy-320-80*j,
              "%i7", sys(Sys_sdl, sdl_joystickgetaxis, @joystick, a)) 
      drawf(30, maxy-340-80*j,
           "Number of buttons   %2i", buttons)
      FOR b = 0 TO buttons-1 DO
        drawf(250+20*b, maxy-340-80*j,
              "%i2", sys(Sys_sdl, sdl_joystickgetbutton, @joystick, b)) 
      drawf(30, maxy-360-80*j,
            "Number of hats      %2i", hats)
      FOR h = 0 TO hats-1 DO
        drawf(250+20*h, maxy-360-80*j,
              "%b4", sys(Sys_sdl, sdl_joystickgethat, @joystick, h))
      sys(Sys_sdl, sdl_joystickclose, @joystick)
    }
  }
}

AND processevents() BE WHILE getevent() SWITCHON eventtype INTO
{ CASE sdle_keydown:
  CASE sdle_quit:      done := TRUE
  DEFAULT:             LOOP
}

LET start() = VALOF
{ initsdl()
  mkscreen("SDL Info", 800, 500)

  done := FALSE

  UNTIL done DO
  { processevents()
    plotscreen()
    updatescreen()
    sdldelay(50)
  }

  writef("*nQuitting*n")
  closesdl()
  RESULTIS 0
}

