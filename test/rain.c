/*
 * $Id: rain.c,v 1.8 1997/05/03 18:38:27 tom Exp $
 */
#include <test.priv.h>

#include <term.h>	/* for tparm() */

#include <signal.h>

/* rain 11/3/1980 EPS/CITHEP */

#define cursor(col,row) move(row,col)

static float ranf(void);
static void onsig(int sig);

int
main(
	int argc GCC_UNUSED,
	char *argv[] GCC_UNUSED)
{
int x, y, j;
static int xpos[5], ypos[5];
float r;
float c;

    for (j=SIGHUP;j<=SIGTERM;j++)
	if (signal(j,SIG_IGN)!=SIG_IGN) signal(j,onsig);

    initscr();
    nl();
    noecho();
    curs_set(0);

    r = (float)(LINES - 4);
    c = (float)(COLS - 4);
    for (j=5;--j>=0;) {
		xpos[j]=(int)(c* ranf())+2;
		ypos[j]=(int)(r* ranf())+2;
    }
    for (j=0;;) {
		x=(int)(c*ranf())+2;
		y=(int)(r*ranf())+2;

		cursor(x,y); addch('.');

		cursor(xpos[j],ypos[j]); addch('o');

		if (j==0) j=4; else --j;
		cursor(xpos[j],ypos[j]); addch('O');

		if (j==0) j=4; else --j;
		cursor(xpos[j],ypos[j]-1);
		addch('-');
		cursor(xpos[j]-1,ypos[j]);
		addstr("|.|");
		cursor(xpos[j],ypos[j]+1);
		addch('-');

		if (j==0) j=4; else --j;
		cursor(xpos[j],ypos[j]-2);
		addch('-');
		cursor(xpos[j]-1,ypos[j]-1);
		addstr("/ \\");
		cursor(xpos[j]-2,ypos[j]);
		addstr("| O |");
		cursor(xpos[j]-1,ypos[j]+1);
		addstr("\\ /");
		cursor(xpos[j],ypos[j]+2);
		addch('-');

		if (j==0) j=4; else --j;
		cursor(xpos[j],ypos[j]-2);
		addch(' ');
		cursor(xpos[j]-1,ypos[j]-1);
		addstr("   ");
		cursor(xpos[j]-2,ypos[j]);
		addstr("     ");
		cursor(xpos[j]-1,ypos[j]+1);
		addstr("   ");
		cursor(xpos[j],ypos[j]+2);
		addch(' ');
		xpos[j]=x; ypos[j]=y;
		refresh();
		napms(50);
    }
}

static void
onsig(int n GCC_UNUSED)
{
    curs_set(1);
    endwin();
    exit(EXIT_FAILURE);
}

static float
ranf(void)
{
    float rv;
    long r = rand();

    r &= 077777;
    rv =((float)r/32767.);
    return rv;
}
