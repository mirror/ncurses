/****************************************************************************
 * Copyright (c) 2017 Free Software Foundation, Inc.                        *
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
 * Author: Thomas E. Dickey
 *
 * $Id: dots_xcurses.c,v 1.8 2017/10/12 00:20:41 tom Exp $
 *
 * A simple demo of the wide-curses interface used for comparison with termcap.
 */
#include <test.priv.h>

#if !defined(__MINGW32__)
#include <sys/time.h>
#endif

#include <time.h>

#if USE_WIDEC_SUPPORT

#if HAVE_ALLOC_PAIR
#define NewPair(n) x_option ? ((void *)&(n)) : NULL
#else
#define NewPair(n) NULL
#endif

#define InitPair(p,fg,bg) init_pair((short) (p), (short) (fg), (short) (bg))

static bool interrupted = FALSE;
static long total_chars = 0;
static time_t started;

#ifdef NCURSES_VERSION
static bool d_option = FALSE;
static bool x_option = FALSE;
#endif

static void
cleanup(void)
{
    endwin();

    printf("\n\n%ld total cells, rate %.2f/sec\n",
	   total_chars,
	   ((double) (total_chars) / (double) (time((time_t *) 0) - started)));
}

static void
onsig(int n GCC_UNUSED)
{
    interrupted = TRUE;
}

static double
ranf(void)
{
    long r = (rand() & 077777);
    return ((double) r / 32768.);
}

static int
mypair(int fg, int bg)
{
    int result;
#if HAVE_ALLOC_PAIR
    if (x_option) {
	result = alloc_pair(fg, bg);
    } else
#endif
    {
	int pair = (fg * COLORS) + bg;
	result = (pair >= COLOR_PAIRS) ? -1 : pair;
    }
    return result;
}

static void
set_colors(int fg, int bg)
{
    int pair = mypair(fg, bg);
    if (pair > 0) {
	color_set((short) pair, NewPair(pair));
    }
}

#if defined(NCURSES_VERSION)
static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: firework [options]"
	,""
	,"Options:"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors()"
#endif
#if HAVE_ALLOC_PAIR
	," -x       use alloc_pair() rather than init_pair()"
#endif
    };
    size_t n;

    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);

    ExitProgram(EXIT_FAILURE);
}
#endif

int
main(int argc GCC_UNUSED,
     char *argv[]GCC_UNUSED)
{
    int margin = 2;
    int x, y, z, p;
    int fg, bg, ch;
    wchar_t wch[2];
    int pair;
    double r;
    double c;

#if defined(NCURSES_VERSION)
    while ((ch = getopt(argc, argv, "dx")) != -1) {
	switch (ch) {
	case 'd':
	    d_option = TRUE;
	    break;
#if HAVE_ALLOC_PAIR
	case 'x':
	    x_option = TRUE;
	    break;
#endif
	default:
	    usage();
	    break;
	}
    }
#endif

    srand((unsigned) time(0));

    InitAndCatch(initscr(), onsig);
    if (has_colors()) {
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (d_option)
	    use_default_colors();
#endif
	if (x_option) {
	    ;			/* nothing */
	} else {
	    for (fg = 0; fg < COLORS; fg++) {
		for (bg = 0; bg < COLORS; bg++) {
		    pair = mypair(fg, bg);
		    if (pair > 0) {
			InitPair(pair, fg, bg);
		    }
		}
	    }
	}
    }

    r = (double) (LINES - (2 * margin));
    c = (double) (COLS - (2 * margin));
    started = time((time_t *) 0);

    fg = COLOR_WHITE;
    bg = COLOR_BLACK;
    pair = 0;
    wch[1] = 0;
    while (!interrupted) {
	x = (int) (c * ranf()) + margin;
	y = (int) (r * ranf()) + margin;
	p = (ranf() > 0.9) ? '*' : ' ';

	move(y, x);
	if (has_colors()) {
	    z = (int) (ranf() * COLORS);
	    if (ranf() > 0.01) {
		set_colors(fg = z, bg);
	    } else {
		set_colors(fg, bg = z);
		napms(1);
	    }
	} else {
	    if (ranf() <= 0.01) {
		if (ranf() > 0.6) {
		    attr_on(WA_REVERSE, NULL);
		} else {
		    attr_off(WA_REVERSE, NULL);
		}
		napms(1);
	    }
	}
	wch[0] = p;
	addnwstr(wch, 1);
	refresh();
	++total_chars;
    }
    cleanup();
    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(void)
{
    printf("This program requires the wide-ncurses library\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
