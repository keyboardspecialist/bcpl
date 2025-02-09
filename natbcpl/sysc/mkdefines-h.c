/*

This is an extension of the previous program mkint-h.c providing some new
features.

This is the program that writes the file defines.h that is included in all the
C sections that consistitue the BCPL Cintcode system.  defines.h contains C
#define statements for macros such as BCPLINT32 and BCPLINT64 for the
architecture that this program is running on.  If the current (not the target)
architecture uses 64 bit pointers, it adds a #define for PTR64. It also
#defines various macro names indicating whether libraries such as SDL, GL and
ALSA are available.

*/

#include <stdio.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <unistd.h>

void str2file(char *str, char *filename) {
  FILE *fd = fopen(filename, "w");
  if (fd) {
    fprintf(fd, "%s", str);
    fclose(fd);
  }
}

int fileexists(const char *filename) {
  struct stat statdata;
  int res = stat(filename, &statdata);
  if(res==0) {
    printf("// file: %s exists\n", filename);
    return -1;
  } else {
    printf("// file: %s does not exist\n", filename);
    return 0;
  }
}
  

int main() {
  int rc = 0;
  printf("/* This file was generated by mkdefines-h */\n\n");

  if (sizeof(int)==4) {
    printf("#define BCPLINT32 int\n");
    printf("#define BCPLUINT32 unsigned int\n");
  } else if (sizeof(long)==4) {
    printf("#define BCPLINT32 long\n");
    printf("#define BCPLUINT32 unsigned long\n");
  }

  if (sizeof(int)==8){
    printf("#define BCPLINT64 int\n");
    printf("#define BCPLUINT64 unsigned int\n");
  } else if (sizeof(long)==8) {
    printf("#define BCPLINT64 long\n");
    printf("#define BCPLUINT64 unsigned long\n");
  } else if (sizeof(long long)==8) {
    printf("#define BCPLINT64 long long\n");
    printf("#define BCPLUINT64 unsigned long long\n");
  }

  // Conditionally define CURRENT64
  if (sizeof(void*)==8){
    printf("#define CURRENT64\n");
    printf("#define ADDRINT BCPLINT64\n");
  }
  else
    printf("#define ADDRINT BCPLINT32\n");

  // Define FLOAT32 and FLOAT64
  printf("#define FLOAT32 float\n");
  printf("#define FLOAT64 double\n");

  if(sizeof(float)!=4) printf("SYSTEM ERROR: sizeof(float) in not 4\n");
  if(sizeof(double)!=8) printf("SYSTEM ERROR: sizeof(double) in not 8\n");

  // TARGET64 is conditionally defined elsewhere depending on the BCPL word
  // length of the target system. BCPLFLOAT will be defined as FLOAT64 or
  // FLOAT32 depending on whether TARGET64 is defined or not. Thus BCPL
  // floating point numbers have the same size as the BCPL word.

  printf("#define BCPLCHAR signed char\n");
  printf("#define UBCPLCHAR unsigned char\n");


#if defined(forLinux)    || \
    defined(forLinux64)  || \
    defined(forRaspi)    || \
    defined(forRaspiSDL) || \
    defined(forRaspiGL)
  if(fileexists("/usr/include/alsa/asoundlib.h"))
  { printf("#define ALSAavail\n");
    rc = system("echo -lasound > ASOUND.txt\n");
  } else {
    rc = system("echo  > ASOUND.txt");
  }

  if(fileexists("/usr/bin/sdl-config"))
  { printf("#define SDLavail\n");
    rc = system("sdl-config --cflags > SDLcflags.txt");
    rc = system("sdl-config --libs   > SDLlibs.txt");
  } else {
    rc = system("echo  > SDLcflags.txt"); // Generate null strings
    rc = system("echo  > SDLlibs.txt");
  }

  // The GL library is only used if the SDL library is installed.
  rc = system("echo > GLlibs.txt");
  if(fileexists("/lib/x86_64-linux-gnu/libGL.so"))
  { printf("#define GLavail\n");
    rc = system("echo \"-L/lib/x86_64-linux-gnu -lGL -lGLU\" > GLlibs.txt");
  }
       

  if(fileexists("/lib/aarch64-linux-gnu/libGL.so"))
  { printf("#define GLavail\n");
    rc = system("echo \"-L/lib/aarch64-linux-gnu -lGL -lGLU\" > GLlibs.txt");
  }

#endif

  return 0;
}
