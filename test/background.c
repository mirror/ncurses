/****************************************************************************
 * Copyright (c) 2003-2006,2011 Free Software Foundation, Inc.              *
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
 * $Id: background.c,v 1.9 2011/04/23 21:16:38 tom Exp $
 */

#define NEED_COLOR_CODE 1
#define NEED_COLOR_NAME 1
#include <color_name.h>

static int default_bg = COLOR_BLACK;
static int default_fg = COLOR_WHITE;

static void
test_background(void)
{
    short f, b;

    pair_content(0, &f, &b);
    printw("pair 0 contains (%d,%d)\n", f, b);
    getch();

    printw("Initializing pair 1 to red/%s\n", color_name(default_bg));
    init_pair(1, COLOR_RED, (short) default_bg);
    bkgdset(' ' | COLOR_PAIR(1));
    printw("RED/BLACK\n");
    getch();

    printw("Initializing pair 2 to %s/blue\n", color_name(default_fg));
    init_pair(2, (short) default_fg, COLOR_BLUE);
    bkgdset(' ' | COLOR_PAIR(2));
    printw("This line should be %s/blue\n", color_name(default_fg));
    getch();

    printw("Resetting colors to pair 0\n");
    bkgdset(' ' | COLOR_PAIR(0));
    printw("Default Colors\n");
    getch();

    printw("Resetting colors to pair 1\n");
    bkgdset(' ' | COLOR_PAIR(1));
    printw("This line should be red/%s\n", color_name(default_bg));
    getch();

    printw("Setting screen to pair 0\n");
    bkgd(' ' | COLOR_PAIR(0));
    getch();

    printw("Setting screen to pair 1\n");
    bkgd(' ' | COLOR_PAIR(1));
    getch();

    printw("Setting screen to pair 2\n");
    bkgd(' ' | COLOR_PAIR(2));
    getch();

    printw("Setting screen to pair 0\n");
    bkgd(' ' | COLOR_PAIR(0));
    getch();
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: background [options]"
	,""
	,"Options:"
#if HAVE_ASSUME_DEFAULT_COLORS
	," -a       invoke assume_default_colors, repeat to use in init_pair"
#endif
	," -b XXX   specify background color"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors, repeat to use in init_pair"
#endif
	," -f XXX   specify foreground color"
    };
    size_t n;

    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);

    ExitProgram(EXIT_FAILURE);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
#if HAVE_ASSUME_DEFAULT_COLORS
    int a_option = 0;
#endif
#if HAVE_USE_DEFAULT_COLORS
    int d_option = 0;
#endif
    int n;

    while ((n = getopt(argc, argv, "ab:df:")) != -1) {
	switch (n) {
#if HAVE_ASSUME_DEFAULT_COLORS
	case 'a':
	    ++a_option;
	    break;
#endif
	case 'b':
	    default_bg = color_code(optarg);
	    break;
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    ++d_option;
	    break;
#endif
	case 'f':
	    default_fg = color_code(optarg);
	    break;
	default:
	    usage();
	}
    }
#if HAVE_USE_DEFAULT_COLORS && HAVE_ASSUME_DEFAULT_COLORS
    if (a_option && d_option) {
	fprintf(stderr, "Use either -a or -d option, but not both\n");
	ExitProgram(EXIT_FAILURE);
    }
#endif

    initscr();
    cbreak();
    noecho();

    if (has_colors()) {
	start_color();

#if HAVE_USE_DEFAULT_COLORS
	if (d_option) {
	    printw("Using default colors...\n");
	    use_default_colors();
	    if (d_option > 1) {
		default_fg = -1;
		default_bg = -1;
	    }
	}
#endif
#if HAVE_ASSUME_DEFAULT_COLORS
	if (a_option) {
	    printw("Using assumed colors %s/%s...\n",
		   color_name(default_fg),
		   color_name(default_bg));
	    assume_default_colors(default_fg, default_bg);
	    if (a_option > 1) {
		default_fg = -1;
		default_bg = -1;
	    }
	}
#endif

	test_background();

    } else {
	printw("This demo requires a color terminal");
	getch();
    }
    endwin();

    ExitProgram(EXIT_SUCCESS);
}
