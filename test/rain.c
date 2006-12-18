/****************************************************************************
 * Copyright (c) 1998-2002,2006 Free Software Foundation, Inc.              *
 *                                                                          *
 * Permission is hereby granted, free of charge, to any person obtaining a  *
 * copy of this software and associated documentation files (the            *
 * "Software"), to deal in the Software without restriction, including      *
 * without limitation the rights to use, copy, modify, merge, publish,      *
 * distribute, distribute with modifications, sublicense, and/or sell       *
 * copies of the Software, and to permit persons to whom the Software is    *
 * furnished to do so, subject to the following conditions:                 *
 *                                                                          *
 * The above copyright notice and this permission notice shall be included  *
 * in all copies or substantial portions of the Software.                   *
 *                                                                          *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  *
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               *
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   *
 * IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   *
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    *
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    *
 * THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               *
 *                                                                          *
 * Except as contained in this notice, the name(s) of the above copyright   *
 * holders shall not be used in advertising or otherwise to promote the     *
 * sale, use or other dealings in this Software without prior written       *
 * authorization.                                                           *
 ****************************************************************************/
/*
 * $Id: rain.c,v 1.22 2006/05/20 15:34:27 tom Exp $
 */
#include <test.priv.h>

/* rain 11/3/1980 EPS/CITHEP */

static float ranf(void);
static void onsig(int sig);

static int
next_j(int j)
{
    if (j == 0)
	j = 4;
    else
	--j;
    if (has_colors()) {
	int z = (int) (3 * ranf());
	chtype color = COLOR_PAIR(z);
	if (z)
	    color |= A_BOLD;
	attrset(color);
    }
    return j;
}

int
main(
	int argc GCC_UNUSED,
	char *argv[]GCC_UNUSED)
{
    int x, y, j;
    static int xpos[5], ypos[5];
    float r;
    float c;

    setlocale(LC_ALL, "");

    CATCHALL(onsig);

    initscr();
    if (has_colors()) {
	int bg = COLOR_BLACK;
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (use_default_colors() == OK)
	    bg = -1;
#endif
	init_pair(1, COLOR_BLUE, bg);
	init_pair(2, COLOR_CYAN, bg);
    }
    nl();
    noecho();
    curs_set(0);
    timeout(0);

    r = (float) (LINES - 4);
    c = (float) (COLS - 4);
    for (j = 5; --j >= 0;) {
	xpos[j] = (int) (c * ranf()) + 2;
	ypos[j] = (int) (r * ranf()) + 2;
    }

    for (j = 0;;) {
	x = (int) (c * ranf()) + 2;
	y = (int) (r * ranf()) + 2;

	mvaddch(y, x, '.');

	mvaddch(ypos[j], xpos[j], 'o');

	j = next_j(j);
	mvaddch(ypos[j], xpos[j], 'O');

	j = next_j(j);
	mvaddch(ypos[j] - 1, xpos[j], '-');
	mvaddstr(ypos[j], xpos[j] - 1, "|.|");
	mvaddch(ypos[j] + 1, xpos[j], '-');

	j = next_j(j);
	mvaddch(ypos[j] - 2, xpos[j], '-');
	mvaddstr(ypos[j] - 1, xpos[j] - 1, "/ \\");
	mvaddstr(ypos[j], xpos[j] - 2, "| O |");
	mvaddstr(ypos[j] + 1, xpos[j] - 1, "\\ /");
	mvaddch(ypos[j] + 2, xpos[j], '-');

	j = next_j(j);
	mvaddch(ypos[j] - 2, xpos[j], ' ');
	mvaddstr(ypos[j] - 1, xpos[j] - 1, "   ");
	mvaddstr(ypos[j], xpos[j] - 2, "     ");
	mvaddstr(ypos[j] + 1, xpos[j] - 1, "   ");
	mvaddch(ypos[j] + 2, xpos[j], ' ');

	xpos[j] = x;
	ypos[j] = y;

	switch (getch()) {
	case ('q'):
	case ('Q'):
	    curs_set(1);
	    endwin();
	    ExitProgram(EXIT_SUCCESS);
	case 's':
	    nodelay(stdscr, FALSE);
	    break;
	case ' ':
	    nodelay(stdscr, TRUE);
	    break;
#ifdef KEY_RESIZE
	case (KEY_RESIZE):
	    r = (float) (LINES - 4);
	    c = (float) (COLS - 4);
	    break;
#endif
	}
	napms(50);
    }
}

static void
onsig(int n GCC_UNUSED)
{
    curs_set(1);
    endwin();
    ExitProgram(EXIT_FAILURE);
}

static float
ranf(void)
{
    long r = (rand() & 077777);
    return ((float) r / 32768.);
}
