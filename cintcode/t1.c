#include <stdio.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <GL/glcorearb.h>

int main() {
  GLuint prog = 0;
  glEnable(1);

  int a = GL_CLEAR;
  printf("Hello a=%d prog=%d\n", a, prog);
  return 0;
}
