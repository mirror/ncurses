/****************************************************************************
 * Copyright 2021 Thomas E. Dickey                                          *
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
 * $Id: combine.c,v 1.7 2021/12/12 01:10:28 tom Exp $
 */

#include <test.priv.h>

#if USE_WIDEC_SUPPORT

static int c_opt;
static int r_opt;

static int
next_char(int value)
{
    do {
	++value;
    } while (!iswprint((wint_t) value));
    return value;
}

static void
do_row(int row, int base_ch, int over_ch)
{
    int col = 0;
    bool done = FALSE;
    bool reverse = (r_opt && !(row % 2));

    move(row, col);
    printw("[U+%04X]", over_ch);
    do {
	if (c_opt) {
	    wchar_t source[2];
	    cchar_t target;
	    attr_t attr = reverse ? A_REVERSE : A_NORMAL;

	    source[1] = 0;

	    source[0] = base_ch;
	    setcchar(&target, source, attr, 0, NULL);
	    add_wch(&target);

	    source[0] = over_ch;
	    setcchar(&target, source, attr, 0, NULL);
	    add_wch(&target);
	} else {
	    wchar_t data[3];

	    data[0] = base_ch;
	    data[1] = over_ch;
	    data[2] = 0;
	    if (reverse)
		attr_on(A_REVERSE, NULL);
	    addwstr(data);
	    if (reverse)
		attr_off(A_REVERSE, NULL);
	}
	col = getcurx(stdscr);
	base_ch = next_char(base_ch);
	done = (col + 1 >= COLS);
    } while (!done);
}

#define LAST_OVER 0x6f

static int
next_over(int value)
{
    if (++value > LAST_OVER)
	value = 0;
    return value;
}

static int
prev_over(int value)
{
    if (--value < 0)
	value = LAST_OVER;
    return value;
}

static void
do_all(int over_it)
{
    int row;

    for (row = 0; row < LINES; ++row) {
	do_row(row, ' ', 0x300 + over_it);
	over_it = next_over(over_it);
    }
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: combine [options]",
	"",
	"Demonstrate combining-characters.",
	"",
	"Options:",
	" -c       use cchar_t data rather than wchar_t string",
	" -r       draw even-numbered rows in reverse-video",
    };
    unsigned n;
    for (n = 0; n < SIZEOF(msg); ++n) {
	fprintf(stderr, "%s\n", msg[n]);
    }
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    int n;
    int over_it = 0;
    bool done = FALSE;

    while ((n = getopt(argc, argv, "cr")) != -1) {
	switch (n) {
	case 'c':
	    c_opt = TRUE;
	    break;
	case 'r':
	    r_opt = TRUE;
	    break;
	default:
	    usage();
	    break;
	}
    }

    setlocale(LC_ALL, "");
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);

    do {
	do_all(over_it);
	switch (getch()) {
	case 'q':
	case QUIT:
	case ESCAPE:
	    done = TRUE;
	    break;
	case KEY_HOME:
	case '0':
	    over_it = 0;
	    break;
	case KEY_END:
	case '$':
	    over_it = LAST_OVER;
	    break;
	case KEY_UP:
	case '-':
	    over_it = prev_over(over_it);
	    break;
	case KEY_DOWN:
	case '+':
	    over_it = next_over(over_it);
	    break;
	}
    } while (!done);

    endwin();

    ExitProgram(EXIT_SUCCESS);
}
#else
int
main(void)
{
    printf("This program requires wide-curses functions\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
