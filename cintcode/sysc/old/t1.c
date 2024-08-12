#include <stdio.h>
#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>
#include <GL/glext.h>

int main() {
  printf("main: entered\n");
  printf("GL_TRUE=%d\n", GL_TRUE);
  GLuint prog = 0;
  prog=glCreateProgram();
  prog=glCreateProgram();
  printf("prog=%d\n", prog);
  return 0;
}
