/* #include <eventque.h> */
#include <grx.h>
#include <grdriver.h>
#include <grxfont.h>
#include <mousex.h>

#define TRUE 1

static char havemouse;
static char checkedformouse = 0;

static void mouse(){
  MouseEvent event;
  if (!checkedformouse) {
    havemouse = MouseDetect();
    checkedformouse = TRUE;
  }
  if (havemouse | TRUE) {
    MouseGetEvent(M_MOTION | M_BUTTON_CHANGE,&event);
  }
}

extern void GrTextXY(int x, int y, char *text, int fg, int bg);
typedef struct {
     unsigned int refs; 
     unsigned int len;
     char array[1];
     } *string;

extern void destroy(char *,int);
extern char *tochars(string s);

void grx_grtext (int x, int y, string s, int fg, int bg) {
  char *p = tochars(s);
  GrTextXY(x,y,p,fg,bg);
  destroy(p,strlen(p)+1);
}
