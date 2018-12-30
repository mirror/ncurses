/****************************************************************************
 * Copyright (c) 2018 Free Software Foundation, Inc.                        *
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
 * $Id: pair_content.c,v 1.2 2018/12/30 00:54:48 tom Exp $
 */

#define NEED_TIME_H
#include <test.priv.h>

typedef struct {
    NCURSES_COLOR_T fg;
    NCURSES_COLOR_T bg;
} MYPAIR;

static int i_opt;
static int l_opt;
static int n_opt;
static int r_opt;
static int s_opt;

static MYPAIR *expected;

#if HAVE_GETTIMEOFDAY
static struct timeval initial_time;
static struct timeval finish_time;
#endif

static void
failed(const char *msg)
{
    printw("%s", msg);
    getch();
    endwin();
    ExitProgram(EXIT_FAILURE);
}

static NCURSES_COLOR_T
random_color(void)
{
    return (NCURSES_COLOR_T) (rand() % COLORS);
}

static void
setup_test(void)
{
    initscr();
    cbreak();
    noecho();
    scrollok(stdscr, TRUE);
    if (has_colors()) {
	start_color();

	if (!l_opt)
	    l_opt = COLOR_PAIRS;
	if (l_opt <= 1)
	    failed("color-pair limit must be greater than one");

	if (!n_opt) {
	    NCURSES_PAIRS_T pair;

	    expected = typeCalloc(MYPAIR, l_opt);
	    if (s_opt) {
		NCURSES_COLOR_T fg;
		NCURSES_COLOR_T bg;
		pair = 1;
		for (fg = 0; fg < COLORS; ++fg) {
		    for (bg = 0; bg < COLORS; ++bg) {
			if (pair < l_opt) {
			    init_pair(pair, fg, bg);
			    expected[pair].fg = fg;
			    expected[pair].bg = bg;
			    ++pair;
			} else {
			    break;
			}
		    }
		}
	    } else {
		for (pair = 1; (int) pair < l_opt; ++pair) {
		    expected[pair].fg = random_color();
		    expected[pair].bg = random_color();
		    init_pair(pair, expected[pair].fg, expected[pair].bg);
		}
	    }
	}
    } else {
	failed("This demo requires a color terminal");
    }
#if HAVE_GETTIMEOFDAY
    gettimeofday(&initial_time, 0);
#endif
}

static void
run_test(void)
{
    NCURSES_PAIRS_T pair;
    bool success = TRUE;
    for (pair = 1; (int) pair < l_opt; ++pair) {
	NCURSES_COLOR_T fg;
	NCURSES_COLOR_T bg;
	if (pair_content(pair, &fg, &bg) == OK) {
	    if (expected != 0) {
		if (fg != expected[pair].fg)
		    success = FALSE;
		if (bg != expected[pair].bg)
		    success = FALSE;
	    }
	}
    }
    if (i_opt) {
	addch(success ? '.' : '?');
	refresh();
    }
}

static void
finish_test(void)
{
    getch();
    endwin();
}

#if HAVE_GETTIMEOFDAY
static double
seconds(struct timeval *mark)
{
    double result = (double) mark->tv_sec;
    result += ((double) mark->tv_usec / 1e6);
    return result;
}
#endif

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: pair_content [options]"
	,""
	,"Options:"
	," -i       interactive, showing test-progress"
	," -l NUM   test NUM color pairs, rather than terminal description"
	," -n       do not initialize color pairs"
	," -r COUNT repeat for given count"
	," -s       initialize pairs sequentially rather than random"
    };
    size_t n;
    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    int i;
    int repeat;

    while ((i = getopt(argc, argv, "il:nr:s")) != -1) {
	switch (i) {
	case 'i':
	    i_opt = 1;
	    break;
	case 'l':
	    if ((l_opt = atoi(optarg)) <= 0)
		usage();
	    break;
	case 'n':
	    n_opt = 1;
	    break;
	case 'r':
	    if ((r_opt = atoi(optarg)) <= 0)
		usage();
	    break;
	case 's':
	    s_opt = 1;
	    break;
	default:
	    usage();
	}
    }
    if (optind < argc)
	usage();
    if (r_opt <= 0)
	r_opt = 1;

    setup_test();

    for (repeat = 0; repeat < r_opt; ++repeat) {
	run_test();
	if (i_opt) {
	    addch('.');
	    refresh();
	}
    }

    if (i_opt) {
	addch('\n');
    }
    printw("DONE: ");
#if HAVE_GETTIMEOFDAY
    gettimeofday(&finish_time, 0);
    printw("%.03f seconds",
	   seconds(&finish_time)
	   - seconds(&initial_time));
#endif
    finish_test();

    ExitProgram(EXIT_SUCCESS);
}
