/****************************************************************************
 * Copyright (c) 1998-2006,2008 Free Software Foundation, Inc.              *
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
 * $Id: rain.c,v 1.26 2008/03/09 00:17:09 tom Exp $
 */
#include <test.priv.h>

/* rain 11/3/1980 EPS/CITHEP */

#ifdef USE_PTHREADS
#include <pthread.h>
#endif

WANT_USE_WINDOW();

#define MAX_DROP 5

struct DATA;

typedef void (*DrawPart) (struct DATA *);

typedef struct DATA {
    int y, x;
#ifdef USE_PTHREADS
    pthread_t thread;
    DrawPart func;
    int state;
#endif
} DATA;

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

static int
random_x(void)
{
    return (((float) (COLS - 4) * ranf()) + 2);
}

static int
random_y(void)
{
    return (((float) (LINES - 4) * ranf()) + 2);
}

static int
next_j(int j)
{
    if (j == 0)
	j = MAX_DROP - 1;
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

static void
part1(DATA * drop)
{
    mvaddch(drop->y, drop->x, '.');
}

static void
part2(DATA * drop)
{
    mvaddch(drop->y, drop->x, 'o');
}

static void
part3(DATA * drop)
{
    mvaddch(drop->y, drop->x, 'O');
}

static void
part4(DATA * drop)
{
    mvaddch(drop->y - 1, drop->x, '-');
    mvaddstr(drop->y, drop->x - 1, "|.|");
    mvaddch(drop->y + 1, drop->x, '-');
}

static void
part5(DATA * drop)
{
    mvaddch(drop->y - 2, drop->x, '-');
    mvaddstr(drop->y - 1, drop->x - 1, "/ \\");
    mvaddstr(drop->y, drop->x - 2, "| O |");
    mvaddstr(drop->y + 1, drop->x - 1, "\\ /");
    mvaddch(drop->y + 2, drop->x, '-');
}

static void
part6(DATA * drop)
{
    mvaddch(drop->y - 2, drop->x, ' ');
    mvaddstr(drop->y - 1, drop->x - 1, "   ");
    mvaddstr(drop->y, drop->x - 2, "     ");
    mvaddstr(drop->y + 1, drop->x - 1, "   ");
    mvaddch(drop->y + 2, drop->x, ' ');
}

#ifdef USE_PTHREADS
static void
napsome(void)
{
    refresh();
    napms(60);
}

/*
 * This runs inside the mutex.
 */
static int
really_draw(WINDOW *win, void *arg)
{
    DATA *data = (DATA *) arg;

    (void) win;
    next_j(data->state);
    data->func(data);
    return OK;
}

static void
draw_part(void (*func) (DATA *), int state, DATA * data)
{
    data->func = func;
    data->state = state;
    use_window(stdscr, really_draw, (void *) data);
    napsome();
}

static void *
draw_drop(void *arg)
{
    DATA mydata;

    mydata = *(DATA *) arg;	/* make a copy of caller's data */

    draw_part(part1, 0, &mydata);
    draw_part(part2, 1, &mydata);
    draw_part(part3, 2, &mydata);
    draw_part(part4, 3, &mydata);
    draw_part(part5, 4, &mydata);
    draw_part(part6, 0, &mydata);

    return NULL;
}
#endif

static int
get_input(void)
{
    int ch;
    ch = USING_WINDOW(stdscr, wgetch);
    return ch;
}

int
main(int argc GCC_UNUSED,
     char *argv[]GCC_UNUSED)
{
    DATA drop;
    DATA last[MAX_DROP];
    int j = 0;
    bool done = FALSE;

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

    for (j = MAX_DROP; --j >= 0;) {
	last[j].x = random_x();
	last[j].y = random_y();
    }

    while (!done) {
	drop.x = random_x();
	drop.y = random_y();

#ifdef USE_PTHREADS
	if (pthread_create(&(drop.thread), NULL, draw_drop, &drop)) {
	    beep();
	    done = TRUE;
	    continue;
	}
#else
	/*
	 * The non-threaded code draws parts of each drop on each loop.
	 */
	part1(&drop);

	part2(&last[j]);

	j = next_j(j);
	part3(&last[j]);

	j = next_j(j);
	part4(&last[j]);

	j = next_j(j);
	part5(&last[j]);

	j = next_j(j);
	part6(&last[j]);

	last[j] = drop;
#endif

	switch (get_input()) {
	case ('q'):
	case ('Q'):
	    done = TRUE;
	    break;
	case 's':
	    nodelay(stdscr, FALSE);
	    break;
	case ' ':
	    nodelay(stdscr, TRUE);
	    break;
#ifdef KEY_RESIZE
	case (KEY_RESIZE):
	    break;
#endif
	}
	napms(50);
    }
    curs_set(1);
    endwin();
    ExitProgram(EXIT_SUCCESS);
}
