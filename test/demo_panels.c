/****************************************************************************
 * Copyright (c) 2007 Free Software Foundation, Inc.                        *
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
 * $Id: demo_panels.c,v 1.5 2007/05/26 22:35:46 tom Exp $
 *
 * Demonstrate a variety of functions from the panel library.
 * Thomas Dickey - 2003/4/26
 */
/*
panel_above			-
panel_below			-
panel_hidden			-
replace_panel			-
*/

#include <test.priv.h>

#if USE_LIBPANEL

#include <panel.h>

typedef void (*InitPanel) (void);
typedef void (*FillPanel) (PANEL *);

static bool use_colors;

static NCURSES_CONST char *mod[] =
{
    "test ",
    "TEST ",
    "(**) ",
    "*()* ",
    "<--> ",
    "LAST "
};

/*+-------------------------------------------------------------------------
	saywhat(text)
--------------------------------------------------------------------------*/
static void
saywhat(NCURSES_CONST char *text)
{
    wmove(stdscr, LINES - 1, 0);
    wclrtoeol(stdscr);
    if (text != 0 && *text != '\0') {
	waddstr(stdscr, text);
	waddstr(stdscr, "; ");
    }
    waddstr(stdscr, "press any key to continue");
}				/* end of saywhat */

static PANEL *
mkpanel(short color, int rows, int cols, int tly, int tlx)
{
    WINDOW *win;
    PANEL *pan = 0;
    char *userdata = malloc(3);

    if ((win = newwin(rows, cols, tly, tlx)) != 0) {
	if ((pan = new_panel(win)) == 0) {
	    delwin(win);
	} else if (use_colors) {
	    short fg = (color == COLOR_BLUE) ? COLOR_WHITE : COLOR_BLACK;
	    short bg = color;

	    init_pair(color, fg, bg);
	    wbkgdset(win, (chtype) (COLOR_PAIR(color) | ' '));
	} else {
	    wbkgdset(win, A_BOLD | ' ');
	}
    }
    sprintf(userdata, "p%d", color % 8);
    set_panel_userptr(pan, (NCURSES_CONST void *) userdata);
    return pan;
}

/*+-------------------------------------------------------------------------
	rmpanel(pan)
--------------------------------------------------------------------------*/
static void
rmpanel(PANEL * pan)
{
    WINDOW *win = panel_window(pan);
    del_panel(pan);
    delwin(win);
}				/* end of rmpanel */

/*+-------------------------------------------------------------------------
	pflush()
--------------------------------------------------------------------------*/
static void
pflush(void)
{
    update_panels();
    doupdate();
}				/* end of pflush */

/*+-------------------------------------------------------------------------
	fill_panel(win)
--------------------------------------------------------------------------*/
static void
init_panel(void)
{
    register int y, x;

    for (y = 0; y < LINES - 1; y++) {
	for (x = 0; x < COLS; x++)
	    wprintw(stdscr, "%d", (y + x) % 10);
    }
}

static void
fill_panel(PANEL * pan)
{
    WINDOW *win = panel_window(pan);
    int num = ((const char *) panel_userptr(pan))[1];
    int y, x;

    wmove(win, 1, 1);
    wprintw(win, "-pan%c-", num);
    wclrtoeol(win);
    box(win, 0, 0);
    for (y = 2; y < getmaxy(win) - 1; y++) {
	for (x = 1; x < getmaxx(win) - 1; x++) {
	    wmove(win, y, x);
	    waddch(win, UChar(num));
	}
    }
}

#if USE_WIDEC_SUPPORT
static void
make_fullwidth_digit(cchar_t *target, int digit)
{
    wchar_t source[2];

    source[0] = digit + 0xff10;
    source[1] = 0;
    setcchar(target, source, A_NORMAL, 0, 0);
}

static void
init_wide_panel(void)
{
    int digit;
    cchar_t temp[10];

    for (digit = 0; digit < 10; ++digit)
	make_fullwidth_digit(&temp[digit], digit);

    do {
	int y, x;
	getyx(stdscr, y, x);
	digit = (y + x / 2) % 10;
    } while (add_wch(&temp[digit]) != ERR);
}

static void
fill_wide_panel(PANEL * pan)
{
    WINDOW *win = panel_window(pan);
    int num = ((const char *) panel_userptr(pan))[1];
    int y, x;

    wmove(win, 1, 1);
    wprintw(win, "-pan%c-", num);
    wclrtoeol(win);
    box(win, 0, 0);
    for (y = 2; y < getmaxy(win) - 1; y++) {
	for (x = 1; x < getmaxx(win) - 1; x++) {
	    wmove(win, y, x);
	    waddch(win, UChar(num));
	}
    }
}
#endif

#define MAX_PANELS 5

static void
canned_panel(PANEL * px[MAX_PANELS + 1], NCURSES_CONST char *cmd)
{
    int which = cmd[1] - '0';

    saywhat(cmd);
    switch (*cmd) {
    case 'h':
	hide_panel(px[which]);
	break;
    case 's':
	show_panel(px[which]);
	break;
    case 't':
	top_panel(px[which]);
	break;
    case 'b':
	bottom_panel(px[which]);
	break;
    case 'd':
	rmpanel(px[which]);
	break;
    }
    pflush();
    wgetch(stdscr);
}

static void
demo_panels(void (*myInit) (void), void (*myFill) (PANEL *))
{
    int itmp;
    PANEL *px[MAX_PANELS + 1];

    scrollok(stdscr, FALSE);	/* we don't want stdscr to scroll! */
    refresh();

    myInit();
    for (itmp = 1; itmp <= MAX_PANELS; ++itmp)
	px[itmp] = 0;

    px[1] = mkpanel(COLOR_RED,
		    LINES / 2 - 2,
		    COLS / 8 + 1,
		    0,
		    0);

    px[2] = mkpanel(COLOR_GREEN,
		    LINES / 2 + 1,
		    COLS / 7,
		    LINES / 4,
		    COLS / 10);

    px[3] = mkpanel(COLOR_YELLOW,
		    LINES / 4,
		    COLS / 10,
		    LINES / 2,
		    COLS / 9);

    px[4] = mkpanel(COLOR_BLUE,
		    LINES / 2 - 2,
		    COLS / 8,
		    LINES / 2 - 2,
		    COLS / 3);

    px[5] = mkpanel(COLOR_MAGENTA,
		    LINES / 2 - 2,
		    COLS / 8,
		    LINES / 2,
		    COLS / 2 - 2);

    myFill(px[1]);
    myFill(px[2]);
    myFill(px[3]);
    myFill(px[4]);
    myFill(px[5]);

    hide_panel(px[4]);
    hide_panel(px[5]);
    pflush();
    saywhat("");
    wgetch(stdscr);

    saywhat("h3 s1 s2 s4 s5");
    move_panel(px[1], 0, 0);
    hide_panel(px[3]);
    show_panel(px[1]);
    show_panel(px[2]);
    show_panel(px[4]);
    show_panel(px[5]);
    pflush();
    wgetch(stdscr);

    canned_panel(px, "s1");
    canned_panel(px, "s2");

    saywhat("m2");
    move_panel(px[2], LINES / 3 + 1, COLS / 8);
    pflush();
    wgetch(stdscr);

    canned_panel(px, "s3");

    saywhat("m3");
    move_panel(px[3], LINES / 4 + 1, COLS / 15);
    pflush();
    wgetch(stdscr);

    canned_panel(px, "b3");
    canned_panel(px, "s4");
    canned_panel(px, "s5");
    canned_panel(px, "t3");
    canned_panel(px, "t1");
    canned_panel(px, "t2");
    canned_panel(px, "t3");
    canned_panel(px, "t4");

    for (itmp = 0; itmp < 6; itmp++) {
	WINDOW *w4 = panel_window(px[4]);
	WINDOW *w5 = panel_window(px[5]);

	saywhat("m4");
	wmove(w4, LINES / 8, 1);
	waddstr(w4, mod[itmp]);
	move_panel(px[4], LINES / 6, itmp * (COLS / 8));
	wmove(w5, LINES / 6, 1);
	waddstr(w5, mod[itmp]);
	pflush();
	wgetch(stdscr);

	saywhat("m5");
	wmove(w4, LINES / 6, 1);
	waddstr(w4, mod[itmp]);
	move_panel(px[5], LINES / 3 - 1, (itmp * 10) + 6);
	wmove(w5, LINES / 8, 1);
	waddstr(w5, mod[itmp]);
	pflush();
	wgetch(stdscr);
    }

    saywhat("m4");
    move_panel(px[4], LINES / 6, itmp * (COLS / 8));
    pflush();
    wgetch(stdscr);

    canned_panel(px, "t5");
    canned_panel(px, "t2");
    canned_panel(px, "t1");
    canned_panel(px, "d2");
    canned_panel(px, "h3");
    canned_panel(px, "d1");
    canned_panel(px, "d4");
    canned_panel(px, "d5");
    canned_panel(px, "d3");

    wgetch(stdscr);

    erase();
    endwin();
}

static void
usage(void)
{
    static const char *const tbl[] =
    {
	"Usage: demo_panels [options]"
	,""
	,"Options:"
	,"  -m       do not use colors"
#if USE_WIDEC_SUPPORT
	,"  -w       use wide-characters in panels and background"
#endif
    };
    size_t n;
    for (n = 0; n < SIZEOF(tbl); n++)
	fprintf(stderr, "%s\n", tbl[n]);
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    int c;
    bool monochrome = FALSE;
    InitPanel myInit = init_panel;
    FillPanel myFill = fill_panel;

    while ((c = getopt(argc, argv, "mw")) != EOF) {
	switch (c) {
	case 'm':
	    monochrome = TRUE;
	    break;
#if USE_WIDEC_SUPPORT
	case 'w':
	    myInit = init_wide_panel;
	    myFill = fill_wide_panel;
	    break;
#endif
	default:
	    usage();
	}
    }

    initscr();

    use_colors = monochrome ? FALSE : has_colors();
    if (use_colors)
	start_color();

    demo_panels(myInit, myFill);
    endwin();

    return EXIT_SUCCESS;
}
#else
int
main(void)
{
    printf("This program requires the curses panel library\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
