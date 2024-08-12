/*
This contains the implemetation of the sys(Sys_gl, fno, ...) facility.

###### Still under development ############

Implemented by Martin Richards (c) Mar 2015

History

05/11/2020
Added gl_deleteBuffer to delete a single buffer.

30/05/2020
Reorganised the conditional compilation macros.

01/05/2020
Making substantion modifications to this file.

06/10/2019
Modified to use floating point values. 32-bit BCPL is assumed.

01/04/2015
Initial implementation


This file is planned to provide and interface to either OpenGL using SDL or
OpenGL ES using EGL (typically for the Raspberry Pi).  To hide the differences
between these two versions of OpenGL, BCPL programs should use the g/gl.b
library with the g/gl.h header file.

Whichever version of OpenGL is used the BCPL interface using

res := sys(Sys_gl, fno, a1, a2, a3, a4,...)

has the same effect.

Note that this calls glfn(args, g, w)
where args[0] = fno, args[1]=a1,... etc
and   g points to the base of the global vector,
and   w points to the base of the Cintcode memory.

fno=0  Test that a version of OpenGL is available
       res is TRUE if it is.

fno=1 ...

Typical values of fno are GL_Init, GL_Quit, or GL_CompileVshader.

This interface currently assumes 32-bit BCPL is being used and most GL calls in
this library are limited to those that pass 32-bit operands.  They thus
correspond to GL types such as GLint, GLuint and GLfloat. The BCPL types are
converted to the GL types where necessary.

In general the manifest constants for fno are essentially the same as the
functions names in GL. Most of these GL calls are implemented simply, as
for instance, the following

  case gl_Uniform4f: // (loc, x, y, z, w)
    return (BCPLWORD) glUniform4f((GLuint) a[1],
                                  N2F(a[2]), N2F(a[3]), N2F(a[4]), N2F(a[5]));

If the GL library is not required, a dummy version of glfn is defined.

SDLavail  is defined if SDL is available.
GLavail   is defined if OpenGL or OpenGL ES libraries are available.
EGLavail  is defined if EGL is available.
GLUTAvail is defined if the GLUT library is available.

Note that RaspiGL uses EGL to call GL but also uses some features provided by
the SDL libraries. SDLavail and EGLavail will never be defined.

*/


#include "cintmain.h"
// Note that cintmain.h conditionally defines quantities such as
// SDLaval and GLavail so must be included early.


#ifndef GLavail
// GL in not required so define a dummy version of glfn.
BCPLWORD glfn(BCPLWORD *args, BCPLWORD *g, BCPLWORD *W) {
  return 0;   // All Sys_gl calls return FALSE if OpenGL is not available
              // This typically only happens in the call: sys(Sys_gl, gl_Init)
}
#endif


#ifdef GLavail
// Define a proper version of glfn.

extern BCPLFLOAT N2F(BCPLWORD  x);
extern BCPLWORD  F2N(BCPLFLOAT x);

// Conversion between BCPL and C strings
extern char *b2c_str(BCPLWORD bstr, char *cstr);
extern BCPLWORD c2b_str(const char *cstr, BCPLWORD bstr);
extern void copyaddrB2C(void*from, void*to);
extern void copyaddrC2B(void*from, void*to);


// The following constants are used by glfn.c and
// they must agree with the declarations in g/gl.h
#define gl_Init                1
#define gl_SetFltScale         2
#define gl_Quit                3
#define gl_GetError            4
#define gl_MkScreen            5
#define gl_SwapBuffers         6
#define gl_MkProg              7
#define gl_CompileVshader      8
#define gl_CompileFshader      9
#define gl_GetAttribLocation  10
#define gl_GetUniformLocation 11
#define gl_DeleteShader       12
#define gl_UseProgram         13
#define gl_LinkProgram        14
#define gl_Uniform1f          15
#define gl_Uniform2f          16
#define gl_Uniform3f          17
#define gl_Uniform4f          18
#define gl_BindAttribLocation 20
#define gl_UniformMatrix4fv   21
#define gl_ClearColour        22
#define gl_ClearBuffer        23
#define gl_M4mulM4            24
#define gl_pollevent          25
#define gl_Enable             26
#define gl_Disable            27
#define gl_DepthFunc          28
#define gl_VertexData         29
#define gl_DrawElements       30
#define gl_EnableVertexAttribArray  31
#define gl_DisableVertexAttribArray 32
#define gl_GenVertexBuffer    33
#define gl_GenIndexBuffer     34
#define gl_VertexAttribPointer 35
#define gl_M4mulV            36
#define gl_ScreenSize        37
#define gl_PrimitiveRestartIndex 38
#define gl_Test              39
#define gl_Clear             40

// Joystick functions -- implemented using SDL
#define gl_numjoysticks       41
#define gl_joystickopen       42
#define gl_joystickclose      43
#define gl_joystickname       44
#define gl_joysticknumaxes    45
#define gl_joysticknumbuttons 46
#define gl_joysticknumballs   47
#define gl_joysticknumhats    48

#define gl_joystickeventstate 49

#define gl_joystickgetbutton  55
#define gl_joystickgetaxis    56
#define gl_joystickgetball    57
#define gl_joystickgethat     58

#define gl_DeleteBuffer       60
#define gl_BlendFunc          61

#ifdef EGLavail
// This code is typically only used when compiling for the Raspberry Pi.
typedef struct
{
   uint32_t screen_width;
   uint32_t screen_height;
// OpenGL|ES objects
   EGLDisplay display;
   EGLSurface surface;
   EGLContext context;

   GLuint verbose;
   GLuint vshader;
   GLuint fshader;
   GLuint mshader;
   GLuint program;
   GLuint program2;
   GLuint tex_fb;
   GLuint tex;
   GLuint buf;
// julia attribs
   GLuint unif_color, attr_vertex, unif_scale, unif_offset, unif_tex, unif_centre; 
// mandelbrot attribs
   GLuint attr_vertex2, unif_scale2, unif_offset2, unif_centre2;
} CUBE_STATE_T;

static CUBE_STATE_T _state, *state=&_state;

#define check() assert(glGetError() == 0)

#endif

#ifdef SDLavail
#include <SDL/SDL.h>

const SDL_VideoInfo* info = NULL;
int width  = 700;
int height = 200;
int bpp = 0;
int flags=0;         // Flags to pass to SDL_SetVideoMode
#endif

#ifdef GLavail
GLuint glProgram=0;
#endif

#ifdef SDLavail
// SDLavail is set when any of the following are set
//    forLinuxSDL, forLinuxGL, forRaspiSDL

// decodeevent is defined in sdlfn.c
extern BCPLWORD decodeevent(SDL_Event*e, BCPLWORD *ptr);
#else
// SDL is not a available so define decodeevents.
BCPLWORD decodeevent(SDL_Event*e, BCPLWORD *ptr) {
  printf("decodeevents should be defined in glfn when SDL is not available\n");
}
#endif


#ifdef GLavail

// This is the proper definition of glfn providing the BCPL interface with
// OpenGL.

BCPLWORD glfn(BCPLWORD *a, BCPLWORD *g, BCPLWORD *W) {
  char tmpstr[256];
  //int argc = 0;

  //printf("glfn: GLavail was defined\n");

  //printf("glfn: Entered: fno=%lld a1=%lld a2=%lld a3=%lld a4=%lld\n",
  //	   LL a[0], LL a[1], LL a[2], LL a[3], LL a[4]);

  switch(a[0]) {
  default:
  { int i = 0;
    printf("glfn: Unknown GL op: "
	   "fno=%lld a1=%lld a2=%lld a3=%lld a4=%lld\n",
	   LL a[0], LL a[1], LL a[2], LL a[3], LL a[4]);
    printf("GL_SRC_ALPHA=%d\n", GL_SRC_ALPHA);
    printf("GL_SRC_ONE_MINUS_SRC_ALPHA=%d\n", GL_ONE_MINUS_SRC_ALPHA);
    printf("GL_BLEND=%d\n", GL_BLEND);
    return 1/i; // Cause and abort
  }

  case gl_Init:
/*
    #ifdef forLinux
    printf("gl_Init: forLinux is set\n");
    #endif
    #ifdef forLinuxSDL
    printf("gl_Init: forLinuxSDL is set\n");
    #endif
    #ifdef forLinuxGL
    printf("gl_Init: forLinuxGL is set\n");
    #endif
    #ifdef forLinuxSDLGL
    printf("gl_Init: forLinuxSDLGL is set\n");
    #endif
    #ifdef forLinuxSDL2GL
    printf("gl_Init: forLinuxSDL2GL is set\n");
    #endif
    #ifdef forRaspi
    printf("gl_Init: forRaspi is set\n");
    #endif
    #ifdef forRaspiSDL
    printf("gl_Init: forRaspiSDL is set\n");
    #endif
    #ifdef forRaspiGL
    printf("gl_Init: forRaspiGL is set\n");
    #endif
    #ifdef forRaspiSDLGL
    printf("gl_Init: forRaspiSDLGL is set\n");
    #endif
    #ifdef forRaspiSDL2GL
    printf("gl_Init: forRaspiSDL2GL is set\n");
    #endif
    
    #ifdef SDLavail
    printf("gl_Init: SDLavail is set\n");
    #endif
    #ifdef EGLavail
    printf("gl_Init: EGLLavail is set\n");
    #endif
    #ifdef GLavail
    printf("gl_Init: GLavail is set\n");
    #endif
    #ifdef ALSAavail
    printf("gl_Init: ALSAavail is set\n");
    #endif
*/
#endif
    
#ifdef forRaspiGLXXX // The OpenGL contxt s now always create using SDL
   // Note that GLavail is only defined when SDLavail is defined.
    
   // On the Raspberry Pi gl_Init creates the OpenGL context using EGL.
   // The SDL library is not loaded.
 { // gl_Init code for the Raspberry Pi.
   int32_t success = 0;
   EGLBoolean result;
   EGLint num_config;

   static EGL_DISPMANX_WINDOW_T nativewindow;

   DISPMANX_ELEMENT_HANDLE_T dispman_element;
   DISPMANX_DISPLAY_HANDLE_T dispman_display;
   DISPMANX_UPDATE_HANDLE_T dispman_update;
   VC_RECT_T dst_rect;
   VC_RECT_T src_rect;

   static const EGLint attribute_list[] =
   {
      EGL_RED_SIZE, 8,
      EGL_GREEN_SIZE, 8,
      EGL_BLUE_SIZE, 8,
      EGL_ALPHA_SIZE, 8,
      EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
      EGL_DEPTH_SIZE, 16,
      EGL_NONE
   };
   
   static const EGLint context_attributes[] = 
   {
      EGL_CONTEXT_CLIENT_VERSION, 2,
      EGL_NONE
   };

   EGLConfig config;

   // RaspiGL uses some SDL features so initialise SDL
   //printf("Calling SDL_Init\n");

   ///{ BCPLWORD res = (BCPLWORD) SDL_Init(SDL_INIT_EVERYTHING);
   ///  if(res!=0)
   ///  { printf("SDL_init failed\n");
   ///    return 0;
   ///  }
   ///  // Enable Unicode translation of keyboard events.
   ///  SDL_EnableUNICODE(1);
   ///  SDL_JoystickEventState(SDL_ENABLE);
   ///  //printf("SDL_Init => %d\n", res);
   ///}

   printf("Calling bcm_host_init()\n");

   bcm_host_init();

   printf("Calling eglGetDisplay(..)\n");

   // Get an EGL display connection
   state->display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
   if(state->display == EGL_NO_DISPLAY) {
     printf("ERROR: eglGetDisplay(..) failed\n");
     return 0;
   }

   // initialize the EGL display connection
   printf("Calling eglInitialize(..)\n");
   result = eglInitialize(state->display, NULL, NULL);
   if(result != EGL_TRUE) {
     printf("ERROR: eglInitialize(..) failed\n");
     return 0;
   }

   // Query the EGL implementation
   printf("Calling eglQueryString(..)\n");
   { const char *str = eglQueryString(state->display, EGL_CLIENT_APIS);
     printf("\nEGL_CLIENT_APIS = %s\n", str);
     str = eglQueryString(state->display, EGL_EXTENSIONS);
     printf("EGL_EXTENSIONS  = %s\n", str);
     str = eglQueryString(state->display, EGL_VENDOR);
     printf("EGL_VENDOR      = %s\n", str);
     str = eglQueryString(state->display, EGL_VERSION);
     printf("EGL_VERSION     = %s\n\n", str);
   }

   // Get an appropriate EGL frame buffer configuration
   //printf("Calling eglChooseConfig(..)\n");
   result = eglChooseConfig(state->display, attribute_list,
			    &config, 1, &num_config);
   assert(EGL_FALSE != result);
   check();

   // Get an appropriate EGL frame buffer configuration
   result = eglBindAPI(EGL_OPENGL_ES_API);
   assert(EGL_FALSE != result);
   check();

   // Create an EGL rendering context
   //printf("Calling eglCreateContext(..)\n");
   state->context = eglCreateContext(state->display, config,
				     EGL_NO_CONTEXT, context_attributes);
   assert(state->context!=EGL_NO_CONTEXT);
   check();

   // Create an EGL window surface
   //printf("Calling graphics_get_display_size(..)\n");
   success = graphics_get_display_size(0 /* LCD */,
				       &state->screen_width,
				       &state->screen_height);
   assert( success >= 0 );

   dst_rect.x = 0;
   dst_rect.y = 0;
   dst_rect.width = state->screen_width;
   dst_rect.height = state->screen_height;
      
   //printf("width=%d  height=%d\n", dst_rect.width, dst_rect.height);

   src_rect.x = 0;
   src_rect.y = 0;
   src_rect.width = state->screen_width << 16;
   src_rect.height = state->screen_height << 16;        

   //printf("Calling vc_dispmanx_display_open(..)\n");

   dispman_display = vc_dispmanx_display_open( 0 /* LCD */);
   dispman_update = vc_dispmanx_update_start( 0 );
         
   dispman_element = vc_dispmanx_element_add(dispman_update,
					     dispman_display,
                                             0/*layer*/, &dst_rect, 0/*src*/,
                                             &src_rect,
					     DISPMANX_PROTECTION_NONE,
					     0 /*alpha*/,
					     0/*clamp*/,
					     0/*transform*/);
      
   nativewindow.element = dispman_element;
   nativewindow.width = state->screen_width;
   nativewindow.height = state->screen_height;
   vc_dispmanx_update_submit_sync( dispman_update );
      
   check();

   state->surface = eglCreateWindowSurface( state->display, config,
					    &nativewindow, NULL );
   assert(state->surface != EGL_NO_SURFACE);
   check();

   // Connect the context to the surface
   result = eglMakeCurrent(state->display, state->surface,
			   state->surface, state->context);
   assert(EGL_FALSE != result);
   check();

   // Set background color and clear buffers
   //glClearColor(0.15f, 0.25f, 0.35f, 1.0f);
   glClearColor(0.95f, 0.65f, 0.35f, 1.0f);
   glClear( GL_COLOR_BUFFER_BIT );

   check();

   eglSwapBuffers(state->display, state->surface);
   check();

   return -1; // Return TRUE
   }
#endif

 //#ifdef forRaspiGL
#ifdef GLavail
    { int argc = 0;
      // forLinuxGL used SDL to create the GL context
      printf("Using SDL to create the OpenGL context\n");
      BCPLWORD res = (BCPLWORD) SDL_Init(SDL_INIT_EVERYTHING);
      //BCPLWORD res = (BCPLWORD) SDL_Init(SDL_INIT_VIDEO);
      if (res<0) {
        fprintf(stderr, "Video initialization failed: %s\n", "error");
		//	SDL_GetError());
        return 0;
        //SDL_Quit();
      }
      
      //printf("glfn: SDL_init returned ok\n");

      info = SDL_GetVideoInfo();
  
      if( !info ) {
        fprintf(stderr, "Video query failed: %s\n",
                SDL_GetError());
        SDL_Quit();
        exit(0);
      }

      bpp = info->vfmt->BitsPerPixel;
      //printf("bpp=%d\n", bpp);

      SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
      SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 5);
      SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);
      SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
      SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

      //printf("Selecting GL Version %d.%d\n", 3, 1);
      //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
      //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);
      //SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK,
      //                    SDL_GL_CONTEXT_PROFILE_MASK);

      return -1; // Return TRUE
    }
#endif


    case gl_Quit:      // Shut down GL
#ifdef SDLavail
      SDL_Quit();
#else
      //printf("Calling eglTerminate(..)\n");
      eglTerminate(state->display);
#endif
      return -1;

#ifdef SDLavail
    case gl_GetError: // Fill str with BCPL string for the latest SDL error
    { char *str = SDL_GetError();
      //printf("sdl_GetError: %s\n", str);
      return c2b_str(str, a[1]); // Convert to BCPL string format
    }
#endif


#ifdef EGLavail
    case gl_MkScreen: // (title, width, height)
      //printf("EGLavail: gl_MkScreen does nothing\n");
      return -1; // Success
#endif

//#ifdef forLinuxGL
#ifdef GLavail
    case gl_MkScreen: // (title, width, height)
    { // Withe forLinuxGL we use SDL to create the OpenGL window
      char tmpstr[256];
      int i;
      char *title = (char *)(&W[a[1]]);
      SDL_Surface *scr;

      width  = a[2];
      height = a[3];

      // Use SDL to create an OpenGL window
      flags = SDL_OPENGL;

      //printf("glfn: SDLavail: gl_MkScreen width=%d height=%d\n",
      //        width, height);

      //printf("Calling SDL_SetVideoMode(%d, %d, %d, %8x)\n",
      //      width, height, bpp, flags);
      scr = SDL_SetVideoMode(width, height, bpp, flags);

      if(scr==0){
        fprintf(stderr, "Video mode set failed: %s\n",
                SDL_GetError());
        SDL_Quit();
        exit(0);
      }

      b2c_str(a[1], tmpstr);

      //printf("gl_MkScreen: title=%s width=%d height=%d\n",
      //      tmpstr, a[2], a[3]);
      SDL_WM_SetCaption(tmpstr, 0);

      // Enable Unicode translation of keyboard events.
      SDL_EnableUNICODE(1);
      // Enable joystick interface evn if there is no joystck.
      SDL_JoystickEventState(SDL_ENABLE);

      //glEnable(GL_BLEND);
      //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      
      //{ SDL_Rect rect = {100, 200, 400, 300};
      //printf("\nfillrect: surface rect=(%d,%d,%d,%d) col=%8x\n",
      //   100, 200, 400, 300, 0x11111111);
      //SDL_FillRect((SDL_Surface*)(scr), &rect, 0x11111111);
      //printf("\nglfn: calling SDL_GL_SwapBuffers\n");
      //SDL_GL_SwapBuffers();
        ////sleep(5);
      //}

      //printf("gl_MkScreen: setting result2=height=%d\n", height);
      g[Gn_result2] = height;
      //printf("gl_MkScreen: returning width=%d\n", width);
      return width;
      //return (BCPLWORD) scr;
    }
#endif

/*
#ifdef GLavail
// Older version.
    { char tmpstr[256];
      int i;
      char *title = (char *)(&W[a[1]]);
      SDL_Surface *scr;

      width  = a[2];
      height = a[3];

      printf("glfn: GLavail: gl_MkScreen width=%d height=%d\n",
	     width, height);

      //printf("Calling SetVideoMode(%d, %d, %d, %8x)\n",
                                     a[1], a[2], a[3], a[4]);
      scr = SDL_SetVideoMode(width, height, bpp, flags);

      if(scr==0){
        fprintf(stderr, "Video mode set failed: %s\n",
                SDL_GetError());
        SDL_Quit();
        exit(0);
      }

      b2c_str(a[1], tmpstr);

      //printf("gl_MkScreen: title=%s width=%d height=%d\n",
      //        tmpstr, a[2], a[3]);
      SDL_WM_SetCaption(tmpstr, 0);

      // Enable Unicode translation of keyboard events.
      SDL_EnableUNICODE(1);
      SDL_JoystickEventState(SDL_ENABLE);

      printf("gl_MkScreen: setting result2 height=%d\n", height);
      g[Gn_result2] = height;
      printf("gl_MkScreen: returning width=%d\n", width);

      { SDL_Rect rect = {100, 200, 400, 300};
    printf("\nfillrect: surface rect=(%d,%d,%d,%d) col=%8x\n",
           100, 200, 400, 300, 0x12345678);
    SDL_FillRect((SDL_Surface*)(scr), &rect, 0x12345678);
      SDL_GL_SwapBuffers();

      }

      return width;
      //return (BCPLWORD) scr;
    }
#endif
*/
      
    case gl_MkProg: // ()
    { GLuint prog =  glCreateProgram();
      //printf("glfn: glCreateProgram => %d\n", prog);
      return (BCPLWORD) prog;
    }

    case gl_CompileVshader: // (prog, cstr)
    { // Compiler the vertex shader whose source is in the
      // C string cstr, and attach it to the given GL program.
      GLuint prog = (GLuint) a[1];
      const char* cstr = (char *) (&W[a[2]]);

      GLuint Vshader = glCreateShader(GL_VERTEX_SHADER);
      //printf("gl_CompileVshader: => %d\n", Vshader);
      glShaderSource(Vshader, 1, &cstr, NULL);
      //printf("gl_CompileVshader: glShaderSource returned\n");
      glCompileShader(Vshader);  // Returns void
      //printf("gl_CompileVshader: glCompileSource returned\n");

      GLint nCompileResult = 0;

      glGetShaderiv(Vshader, GL_COMPILE_STATUS, &nCompileResult);
      //printf("gl_CompileVshader: nCompileResult=%d\n", nCompileResult);

      if(!nCompileResult)
      { int i;
        char Log[1024];
        GLint nLength;
	printf("glGetShaderiv failed\n");
        glGetShaderInfoLog(Vshader, 1024, &nLength, Log);
        for(i=0; i<nLength; i++) printf("%c", Log[i]);
        printf("\n");
      }      

      //printf("gl_CompileVshader: Calling glAttachShader\n");
      glAttachShader(prog, Vshader);
      //printf("gl_CompileVshader: Returned from glAttachShader => %d\n",
      //        Vshader);
      return Vshader;
    }

    case gl_CompileFshader: // (prog, cstr)
    { // Compiler the fragment shader whose source is in the
      // C string cstr, and attach it to the given GL program.
      GLuint prog = (GLuint) a[1];
      const char* cstr = (char *) (&W[a[2]]);
      GLuint Fshader = glCreateShader(GL_FRAGMENT_SHADER);
      //printf("gl_CompileFshader: => %d\n", Fshader);
      glShaderSource(Fshader, 1, &cstr, NULL);
      //printf("gl_CompileFshader: glShaderSource returned\n");
      glCompileShader(Fshader);  // Returns null
      //printf("gl_CompileFshader: glCompileSource returned\n");

      GLint nCompileResult = 0;

      glGetShaderiv(Fshader, GL_COMPILE_STATUS, &nCompileResult);
      //printf("gl_CompileFshader: nCompileResult=%d\n", nCompileResult);

      if(!nCompileResult)
      { int i;
        char Log[1024];
        GLint nLength=20;
        glGetShaderInfoLog(Fshader, 1024, &nLength, Log);
        for(i=0; i<nLength; i++) printf("%c", Log[i]);
        printf("\n");
      }      

      //printf("gl_CompileFshader: Calling glAttachShader\n");
      glAttachShader(prog, Fshader);
      //printf("gl_CompileFshader: Returned from glAttachShader => %d\n",
      //        Fshader);
      return Fshader;
    }

    case gl_LinkProgram: // (prog)
    { // Return -1 if successful.
      GLuint prog = (GLuint)a[1];
      glLinkProgram(prog);

      GLint nLinkResult = 0;

      glGetProgramiv(prog, GL_LINK_STATUS, &nLinkResult);

      if(!nLinkResult)
      { int i;
        char Log[1024];
        GLint nLength;
        glGetProgramInfoLog(prog, 1024, &nLength, Log);
        for(i=0; i<nLength; i++) printf("%c", Log[i]);
        printf("\n");
      }
      //printf("glfn: gl_LinkProgram returning -1\n");
      return -1; // Successful return
    }


    case gl_BindAttribLocation: // (prog, loc, name)
    { // Specify the location of an attribute before linking
      GLuint prog = (GLuint) a[1];
      GLuint loc  = (GLuint) a[2];
      b2c_str(a[3], tmpstr);
      printf("glfn: BindAttribLocation prog=%d loc=%d name=%s\n",
	     prog, loc, tmpstr);
      return 0;///(BCPLWORD) glBindAttribLocation(prog, loc, tmpstr);
    }

    case gl_Uniform1f: // (loc, x)  Set 1 uniform element
      glUniform1f((GLuint) a[1], N2F(a[2]));
      return 0;

    case gl_Uniform2f: // (loc, x, y)   Set 2 uniform elements
      glUniform2f((GLuint) a[1], N2F(a[2]), N2F(a[3]));
      return 0;

    case gl_Uniform3f: // (loc, x, y, z)  // Set 3 uniform elements
      glUniform3f((GLuint) a[1], N2F(a[2]), N2F(a[3]), N2F(a[4]));
      return 0;

    case gl_Uniform4f: // (loc, x, y, z, w)  // Set 4 uniform elements
      glUniform4f((GLuint) a[1], N2F(a[2]), N2F(a[3]), N2F(a[4]), N2F(a[5]));
      return 0;

    case gl_GetAttribLocation: // (prog, name)
      // Find out where the linker put an attribute variable
      return (BCPLWORD) glGetAttribLocation((GLuint) a[1],
					     b2c_str(a[2], tmpstr));

    case gl_GetUniformLocation: // (prog, name)
      // Find out where the linker put a uniform variable
      return (BCPLWORD) glGetUniformLocation((GLuint) a[1],
                                              b2c_str(a[2], tmpstr));

    case gl_UniformMatrix4fv: // (loc, prog, matrix) -- 4x4 matrix
    { glUniformMatrix4fv((GLuint) a[1],
                         (GLuint) a[2],
                         GL_FALSE,
                         (GLfloat *) (&W[a[3]]));
      return -1;
    }

    case gl_DeleteShader: // (shader)
    { glDeleteShader((GLuint) a[1]);
      return -1;
    }
    case gl_UseProgram: // (prog)
    { glUseProgram((GLuint) a[1]);
      return -1;
    }
    case gl_Enable: // (op)
    { glEnable((GLint)a[1]);
      return -1;
    }

    case gl_Disable: // (op)
    { glDisable((GLint)a[1]);
      return -1;
    }

    case gl_BlendFunc: // (src, dest)
    { glBlendFunc((GLint)a[1], (GLint)a[2]);
      return -1;
    }

    case gl_DepthFunc: // (relation)
    { glDepthFunc((GLuint)a[1]);
      return -1;
    }

    case gl_VertexData: // (loc, n, stride, offset))
    { // This copied vertex values from client to GL memory.
      // loc is the location of the vertex shader input variable
      // used to access this data.
      // n is the number of words in the value.
      // stride is the size of the vertex in words
      // offset is the word offset of the data in a vertex.

      glVertexAttribPointer((GLint) a[1],    // Location value of the vertex
                                             // shader input variable.

                            (GLint) a[2],    // n elements
			    GL_FLOAT,        //   always of type float
                            GL_FALSE,        // Do not normalise
                            (GLint)(a[3]*4), // Stride in bytes
			    (void *)(ADDRINT)(GLint)(a[4]*4)); // offset

      glEnableVertexAttribArray((GLint) a[1]); // Enable the shader variable.
      return -1;
    }

    case gl_DrawElements: // (mode, count, offset)
      // mode   is 1=points, 2=lines, 3=linestrip, etc.
      //           4=linellop, 5=triangles, 6=tranglestrip,
      //           7=trianglefan.
      // count  is the number of index values to use.
      // offset is the subscript position of the first index vector
      //           element to use. The elements are 32-bit integers.


      glDrawElements((GLenum)(a[1]),    // mode
                     (GLsizei)(a[2]),   // Number of elements to use
                     GL_UNSIGNED_INT,   // Type of the index elements
                     (GLint*)(ADDRINT)(4*a[3]));// The position in bytes
                                        // (cast as a pointer) of
                                        // the first element of
                                        // the index vector to use.
      return -1;  // Successful return


    case gl_EnableVertexAttribArray: // (attrib)
      glEnableVertexAttribArray((GLint)(a[1]));
      return -1;

    case gl_DisableVertexAttribArray: // (attrib)
      glDisableVertexAttribArray((GLint)(a[1]));
      return -1;

    case gl_GenVertexBuffer: // (size, data)
    { // Generate a new buffer object 'name'
      // Bind it and fill it with data
      GLint size = (GLint)a[1];            // The number of floats in data,
      GLfloat *data = (GLfloat *)&W[a[2]]; // data points to 32-bit floats
      GLuint buffer;
      glGenBuffers(1, &buffer);            // Allocate a buffer object
      glBindBuffer(GL_ARRAY_BUFFER, buffer);
      glBufferData(GL_ARRAY_BUFFER,        // Copy data to graphics memory
                   size * sizeof(GLfloat), // The size of data in bytes
                   data,                   // The vertex data
                   GL_STATIC_DRAW);        // Usage hint

      return (BCPLWORD)buffer;
    }

    case gl_GenIndexBuffer: // (size, data) // Order changed 26/11/2020
    { // Generate a new index buffer object 'name'
      // Bind it and fill it with data
      GLint size = (GLint)a[1]; // Number of 32-bit indices
      GLuint *data = (GLuint *)&W[a[2]];
      GLuint buffer;
      glGenBuffers(1, &buffer);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffer);
      glBufferData(GL_ELEMENT_ARRAY_BUFFER, // Copy index data to GL  memory
                   size * sizeof(GLuint),   // The number data bytes to copy.
                   data,                    // The vector of index data
                   GL_STATIC_DRAW);         // Usage hint
      return (BCPLWORD)buffer;
    }

    case gl_DeleteBuffer: // (buffer)
    { // Delete a vertex or index buffer.
      // buffer=0 is OK but does nothing. 
      GLuint buffer = (GLuint)a[1];
      glDeleteBuffers(1, &buffer);
      return -1;
    }

    case gl_ClearColour: // (r, g, b, a)  all floats in range 0.0 to 1.0
      glClearColor(N2F(a[1]), N2F(a[2]), N2F(a[3]), N2F(a[4]));
      return -1;

    case gl_Clear: // (bits)
      glClear((GLbitfield) a[1]);
      return -1;

    case gl_ClearBuffer: // ()    Obsolete -- replace by a call of gl_Clear
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      //glClear(GL_COLOR_BUFFER_BIT);
      return -1;

/*
    case gl_Test:               // 39 ()
    { int i;
      printf("\ngl_test\n");

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      //gluOrtho2D(0.0, 640.0, 0.0, 480.0);

      for (i = 0; i<20; i++) {
        int x = rand()%640; 
        int y = rand()%480;

        float r=(float)((rand() % 9))/8;
        float g=(float)((rand() % 9))/8;
        float b=(float)((rand() % 9))/8;

        printf("x=%3d y=%3d r=%6.3f r=%6.3f r=%6.3f\n", x,y, r,g,b);
      
        //glColor3f(r,g,b); 	

        //glBegin(GL_POINTS);
        //glVertex2i (x,y);
        //glEnd();

        glFlush();
        sleep(2);

      }
  
      printf("Return from gl_Test\n");
      return -1;
    }
*/
      
    case gl_SwapBuffers: // ()

#ifdef forRaspiGLXXX  // Only use SDL to swap buffers
      eglSwapBuffers(state->display, state->surface);
      check();
      return -1;
#else
      SDL_GL_SwapBuffers();
      return -1;
#endif


    case gl_pollevent: // (pointer) to [type, args, ... ] to hold
          	       // details of the next event
#ifdef forRaspiGLXXX
      printf("gl_pollevent not yet implemented for Raspberry Pi\n");
      ///{ SDL_Event test_event;
      ///if (SDL_PollEvent(&test_event))
      ///{ decodeevent(&test_event, &W[a[1]]);
      ///  return -1;
      ///}
      ///decodeevent(0, &W[a[1]]);
      return 0;
#else
    { SDL_Event test_event;
      if (SDL_PollEvent(&test_event))
      { decodeevent(&test_event, &W[a[1]]);
        return -1;
      }
      decodeevent(0, &W[a[1]]);
      return 0;
    }
#endif


    case gl_M4mulM4: // (A, B, C) performs C := A * B
    { float *A = (float *)(&W[a[1]]);
      float *B = (float *)(&W[a[2]]);
      float *C = (float *)(&W[a[3]]);
      
      // aij    i is the row number
      //        j is the column number
      // The calculation required is
      // cij = ai0*b0j + ai1*b1j + ai2*b2j + ai3*b3j
      
      float a00=A[ 0], a10=A[ 1], a20=A[ 2], a30=A[ 3];
      float a01=A[ 4], a11=A[ 5], a21=A[ 6], a31=A[ 7];
      float a02=A[ 8], a12=A[ 9], a22=A[10], a32=A[11];
      float a03=A[12], a13=A[13], a23=A[14], a33=A[15];

      float b00=B[ 0], b10=B[ 1], b20=B[ 2], b30=B[ 3];
      float b01=B[ 4], b11=B[ 5], b21=B[ 6], b31=B[ 7];
      float b02=B[ 8], b12=B[ 9], b22=B[10], b32=B[11];
      float b03=B[12], b13=B[13], b23=B[14], b33=B[15];

      //printf("\ngl_M4mulM4: Multiplication details\n");

      //printf("%11.6f %11.6f %11.6f %11.6f \n",   a00, a01, a02, a03);
      //printf("%11.6f %11.6f %11.6f %11.6f \n",   a10, a11, a12, a13);
      //printf("%11.6f %11.6f %11.6f %11.6f \n",   a20, a21, a22, a23);
      //printf("%11.6f %11.6f %11.6f %11.6f \n\n", a30, a31, a32, a33);

      //printf("%11.6f %11.6f %11.6f %11.6f \n",   b00, b01, b02, b03);
      //printf("%11.6f %11.6f %11.6f %11.6f \n",   b10, b11, b12, b13);
      //printf("%11.6f %11.6f %11.6f %11.6f \n",   b20, b21, b22, b23);
      //printf("%11.6f %11.6f %11.6f %11.6f \n\n", b30, b31, b32, b33);

      C[ 0] = a00*b00 + a01*b10 + a02*b20 + a03*b30; // c00
      C[ 1] = a10*b00 + a11*b10 + a12*b20 + a13*b30; // c10
      C[ 2] = a20*b00 + a21*b10 + a22*b20 + a23*b30; // c20
      C[ 3] = a30*b00 + a31*b10 + a32*b20 + a33*b30; // c30

      C[ 4] = a00*b01 + a01*b11 + a02*b21 + a03*b31; // c01
      C[ 5] = a10*b01 + a11*b11 + a12*b21 + a13*b31; // c11
      C[ 6] = a20*b01 + a21*b11 + a22*b21 + a23*b31; // c21
      C[ 7] = a30*b01 + a31*b11 + a32*b21 + a33*b31; // c31

      C[ 8] = a00*b02 + a01*b12 + a02*b22 + a03*b32; // c02
      C[ 9] = a10*b02 + a11*b12 + a12*b22 + a13*b32; // c12
      C[10] = a20*b02 + a21*b12 + a22*b22 + a23*b32; // c22
      C[11] = a30*b02 + a31*b12 + a32*b22 + a33*b32; // c32

      C[12] = a00*b03 + a01*b13 + a02*b23 + a03*b33; // c03
      C[13] = a10*b03 + a11*b13 + a12*b23 + a13*b33; // c13
      C[14] = a20*b03 + a21*b13 + a22*b23 + a23*b33; // c23
      C[15] = a30*b03 + a31*b13 + a32*b23 + a33*b33; // c33

      //printf("%11.6f %11.6f %11.6f %11.6f \n",   C[0], C[4], C[ 8], C[12]);
      //printf("%11.6f %11.6f %11.6f %11.6f \n",   C[1], C[5], C[ 9], C[13]);
      //printf("%11.6f %11.6f %11.6f %11.6f \n",   C[2], C[6], C[10], C[14]);
      //printf("%11.6f %11.6f %11.6f %11.6f \n\n", C[3], C[7], C[11], C[15]);
      return 0;
    }

    case gl_M4mulV: // (A, B, C) performs C := A * B
                    // where A is a 4x4 matrix and B and C are
                    // 4 element vectors. B and C need not be distinct.
    { float *A = (float *)(&W[a[1]]);
      float *B = (float *)(&W[a[2]]);
      float *C = (float *)(&W[a[3]]);

      float a00=A[ 0], a10=A[ 1], a20=A[ 2], a30=A[ 3];
      float a01=A[ 4], a11=A[ 5], a21=A[ 6], a31=A[ 7];
      float a02=A[ 8], a12=A[ 9], a22=A[10], a32=A[11];
      float a03=A[12], a13=A[13], a23=A[14], a33=A[15];

      float b0=B[0], b1=B[1], b2=B[2], b3=B[3];

      C[0] = a00*b0 + a01*b1 + a02*b2 + a03*b3; // c0
      C[1] = a10*b0 + a11*b1 + a12*b2 + a13*b3; // c1
      C[2] = a20*b0 + a21*b1 + a22*b2 + a23*b3; // c2
      C[3] = a30*b0 + a31*b1 + a32*b2 + a33*b3; // c3

      return 0;
    }

    case gl_PrimitiveRestartIndex:
    { GLuint w = (GLuint)(a[1]);
      //glEnable(GL_PRIMITIVE_RESTART);
      printf("PrimitiveRestart: w=%4x NOT AVAILABLE\n", w);
      ///glPrimitiveRestartIndex(w);
      return -1;
    }

    case gl_ScreenSize: // (@xsize, @ysize)
      printf("glfn: gl_ScreenSize called\n");
#ifdef forRaspiGLXXX
      W[a[2]] = state->screen_width;
      W[a[3]] = state->screen_height;
#endif
      return -1;

#ifdef SDLavail
// Joystick functions currently provided by sdlfn.c
  case gl_numjoysticks:
    return SDL_NumJoysticks();

  case gl_joystickopen:       // 42 (index, joyptr) => joy
    { SDL_Joystick *joystick = SDL_JoystickOpen(a[1]);
      copyaddrC2B(&joystick, &W[a[2]]);
      return 0;
    }

  case gl_joystickclose:      // 43 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    SDL_JoystickClose(joystick);
    return 0;
  }
  
  case gl_joystickname:       // 44 (index, name)
  { const char *name = SDL_JoystickName(a[1]);
    return c2b_str(name, a[2]);
  }

  case gl_joysticknumaxes:    // 45 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return (BCPLWORD)SDL_JoystickNumAxes(joystick);
  }
  
  case gl_joysticknumbuttons: // 46 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickNumButtons(joystick);
  }
  
  case gl_joysticknumballs:   // 47 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickNumBalls(joystick);
  }
  
  case gl_joysticknumhats:    // 47 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickNumHats(joystick);
  }
      
  case gl_joystickeventstate: //49  sdl_enable=1 or sdl_ignore=0
    return SDL_JoystickEventState(a[1]);

  case gl_joystickgetbutton:  // 55 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickGetButton(joystick, a[2]);
  }
  
  case gl_joystickgetaxis:    // 56 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickGetAxis(joystick, a[2]);
  }
  
  case gl_joystickgethat:     // 58 (joyptr)
  { SDL_Joystick *joystick;
    copyaddrB2C(&W[a[1]], &joystick);
    return SDL_JoystickGetHat(joystick, a[2]);
  }
#endif
  }  // End of switch
}
#endif



