/****************************************************************************
 * Copyright 2022 Leonid S. Usov <leonid.s.usov at gmail.com>               *
 * Copyright 2022 Thomas E. Dickey                                          *
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
 ****************************************************************************/
/*
 * $Id: test_mouse.c,v 1.8 2022/05/08 00:36:07 tom Exp $
 *
 * Author: Leonid S Usov
 *
 * Observe mouse events in the raw terminal or parsed ncurses modes
 */

#include <test.priv.h>

#if defined(NCURSES_MOUSE_VERSION) && !defined(_NC_WINDOWS)

static int logoffset = 0;

static int
raw_loop(void)
{
    struct termios tty;
    struct termios old;
    char *xtermcap;

    tcgetattr(0, &old);
    cfmakeraw(&tty);

    setupterm(NULL, 0, 0);
    xtermcap = tigetstr("XM");
    if (xtermcap == 0 || xtermcap == (char *) -1) {
	fprintf(stderr, "couldn't get XM terminfo");
	return 1;
    }

    putp(tparm(xtermcap, 1));
    fflush(stdout);

    tcsetattr(0, TCSANOW, &tty);

    while (true) {
	int c = getc(stdin);
	const char *pretty;

	if (c == ERR || c == '\003') {
	    break;
	} else if (c == '\033') {
	    printf("\r\n");
	} else if ((pretty = unctrl((chtype) c)) != NULL) {
	    printf("%s", pretty);
	} else if (isprint(c)) {
	    printf("%c", c);
	} else {
	    printf("{%x}", c);
	}
    }

    putp(tparm(xtermcap, 0));
    fflush(stdout);
    tcsetattr(0, TCSANOW, &old);
    return 0;
}

static int logw(int line, const char *fmt, ...) GCC_PRINTFLIKE(2, 3);

static int
logw(int line, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    wmove(stdscr, line++, 0);
    vw_printw(stdscr, fmt, args);
    clrtoeol();

    line %= (getmaxy(stdscr) - logoffset);
    if (line < logoffset) {
	line = logoffset;
    }

    wmove(stdscr, line, 0);
    wprintw(stdscr, ">");
    clrtoeol();
    return line;
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: test_mouse [options]",
	"",
	"Test mouse events.  These examples for $TERM demonstrate xterm"
	"features:",
	"    xterm",
	"    xterm-1002",
	"    xterm-1003",
	"",
	"Options:",
	" -r       show raw input stream, injecting a new line before every ESC",
	" -i n     set mouse interval to n; default is 0",
	" -h       show this message",
	" -T term  use terminal description other than $TERM"
    };
    unsigned n;
    for (n = 0; n < sizeof(msg) / sizeof(char *); ++n) {
	fprintf(stderr, "%s\n", msg[n]);
    }
}

int
main(int argc, char *argv[])
{
    bool rawmode = FALSE;
    int interval = 0;
    int curline;
    int c;
    MEVENT event;
    char *my_environ;
    const char *term_format = "TERM=%s";

    while ((c = getopt(argc, argv, "hi:rT:")) != -1) {
	switch (c) {
	case 'h':
	    usage();
	    ExitProgram(EXIT_SUCCESS);
	case 'i':
	    interval = atoi(optarg);
	    break;
	case 'r':
	    rawmode = TRUE;
	    break;
	case 'T':
	    my_environ = malloc(strlen(term_format) + strlen(optarg));
	    sprintf(my_environ, term_format, optarg);
	    putenv(my_environ);
	    break;
	default:
	    usage();
	    ExitProgram(EXIT_FAILURE);
	}
    }
    if (optind < argc) {
	usage();
	ExitProgram(EXIT_FAILURE);
    }

    if (rawmode) {
	printf("Entering raw mode. Ctrl-c to quit.\n");
	return raw_loop();
    }

    initscr();
    clear();
    noecho();
    cbreak();			/* Line buffering disabled; pass everything */
    nonl();
    keypad(stdscr, TRUE);

    /* Get all the mouse events */
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
    mouseinterval(interval);

    logoffset = logw(logoffset, "Ctrl-c to quit");
    logoffset = logw(logoffset, "--------------");

    curline = logoffset;

    while (1) {
	c = getch();

	switch (c) {
	case KEY_MOUSE:
	    if (getmouse(&event) == OK) {
		unsigned btn;
		mmask_t events;
#if NCURSES_MOUSE_VERSION > 1
		const int max_btn = 5;
#else
		const int max_btn = 4;
#endif
		for (btn = 1; btn <= max_btn; btn++) {
		    events = (mmask_t) (event.bstate
					& NCURSES_MOUSE_MASK(btn,
							     NCURSES_BUTTON_RELEASED |
							     NCURSES_BUTTON_PRESSED |
							     NCURSES_BUTTON_CLICKED |
							     NCURSES_DOUBLE_CLICKED |
							     NCURSES_TRIPLE_CLICKED));
		    if (events == 0)
			continue;
#define Show(btn,name) ((event.bstate & NCURSES_MOUSE_MASK(btn, name)) != 0) ? #name : ""
		    curline = logw(curline,
				   "button %d %s %s %s %s %s %d[%x] @ %d, %d",
				   btn,
				   Show(btn, NCURSES_BUTTON_RELEASED),
				   Show(btn, NCURSES_BUTTON_PRESSED),
				   Show(btn, NCURSES_BUTTON_CLICKED),
				   Show(btn, NCURSES_DOUBLE_CLICKED),
				   Show(btn, NCURSES_TRIPLE_CLICKED),
				   (event.bstate & REPORT_MOUSE_POSITION) != 0,
				   events,
				   event.y, event.x);
		}
	    }
	    break;
	case '\003':
	    goto end;
	default:
	    curline = logw(curline, "got another char: 0x%x", c);
	}
	refresh();
    }
  end:
    endwin();
    ExitProgram(EXIT_SUCCESS);
}
#else
int
main(void)
{
    printf("This program requires the ncurses library\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
