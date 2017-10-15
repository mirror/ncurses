/****************************************************************************
 * Copyright (c) 1998-2016,2017 Free Software Foundation, Inc.              *
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
 * view.c -- a silly little viewer program
 *
 * written by Eric S. Raymond <esr@snark.thyrsus.com> December 1994
 * to test the scrolling code in ncurses.
 *
 * modified by Thomas Dickey <dickey@clark.net> July 1995 to demonstrate
 * the use of 'resizeterm()', and May 2000 to illustrate wide-character
 * handling.
 *
 * Takes a filename argument.  It's a simple file-viewer with various
 * scroll-up and scroll-down commands.
 *
 * n	-- scroll one line forward
 * p	-- scroll one line back
 *
 * Either command accepts a numeric prefix interpreted as a repeat count.
 * Thus, typing `5n' should scroll forward 5 lines in the file.
 *
 * The way you can tell this is working OK is that, in the trace file,
 * there should be one scroll operation plus a small number of line
 * updates, as opposed to a whole-page update.  This means the physical
 * scroll operation worked, and the refresh() code only had to do a
 * partial repaint.
 *
 * $Id: view.c,v 1.111 2017/10/15 00:56:58 tom Exp $
 */

#include <test.priv.h>
#include <widechars.h>
#include <popup_msg.h>

#include <sys/stat.h>
#include <time.h>

#undef CTRL			/* conflict on AIX 5.2 with <sys/ioctl.h> */

static void finish(int sig) GCC_NORETURN;

#if HAVE_TERMIOS_H
# include <termios.h>
#else
#if !defined(__MINGW32__)
# include <sgtty.h>
#endif
#endif

#if !defined(sun) || !HAVE_TERMIOS_H
# if HAVE_SYS_IOCTL_H
#  include <sys/ioctl.h>
# endif
#endif

#define my_pair 1

/* This is needed to compile 'struct winsize' */
#if NEED_PTEM_H
#include <sys/stream.h>
#include <sys/ptem.h>
#endif

#undef CTRL
#define CTRL(x)	((x) & 0x1f)

#if defined(SIGWINCH) && defined(TIOCGWINSZ) && HAVE_RESIZE_TERM
#define CAN_RESIZE 1
#else
#define CAN_RESIZE 0
#endif

#if CAN_RESIZE
static int interrupted;
static bool waiting = FALSE;
#endif

static int shift = 0;
static bool try_color = FALSE;

static char *fname;
static NCURSES_CH_T **vec_lines;
static NCURSES_CH_T **lptr;
static int num_lines;

static void usage(void) GCC_NORETURN;

static void
failed(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    ExitProgram(EXIT_FAILURE);
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: view [options] file"
	,""
	,"Options:"
	," -c       use color if terminal supports it"
	," -i       ignore INT, QUIT, TERM signals"
#if defined(KEY_RESIZE)
	," -r       use old-style sigwinch handler rather than KEY_RESIZE"
#endif
	," -s       start in single-step mode, waiting for input"
#ifdef TRACE
	," -t       trace screen updates"
	," -T NUM   specify trace mask"
#endif
    };
    size_t n;
    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);
    ExitProgram(EXIT_FAILURE);
}

static int
ch_len(NCURSES_CH_T * src)
{
    int result = 0;
#if USE_WIDEC_SUPPORT
    int count;
#endif

#if USE_WIDEC_SUPPORT
    for (;;) {
	TEST_CCHAR(src, count, {
	    ++result;
	    ++src;
	}
	, {
	    break;
	})
    }
#else
    while (*src++)
	result++;
#endif
    return result;
}

/*
 * Allocate a string into an array of chtype's.  If UTF-8 mode is
 * active, translate the string accordingly.
 */
static NCURSES_CH_T *
ch_dup(char *src)
{
    unsigned len = (unsigned) strlen(src);
    NCURSES_CH_T *dst = typeMalloc(NCURSES_CH_T, len + 1);
    size_t j, k;
#if USE_WIDEC_SUPPORT
    wchar_t wstr[CCHARW_MAX + 1];
    wchar_t wch;
    int l = 0;
    size_t rc;
    int width;
#ifndef state_unused
    mbstate_t state;
#endif
#endif /* USE_WIDEC_SUPPORT */

#if USE_WIDEC_SUPPORT
    reset_mbytes(state);
#endif
    for (j = k = 0; j < len; j++) {
#if USE_WIDEC_SUPPORT
	rc = (size_t) check_mbytes(wch, src + j, len - j, state);
	if (rc == (size_t) -1 || rc == (size_t) -2) {
	    break;
	}
	j += rc - 1;
	width = wcwidth(wch);
	if (width == 0) {
	    if (l == 0) {
		wstr[l++] = L' ';
	    }
	} else if ((l > 0) || (l == CCHARW_MAX)) {
	    wstr[l] = L'\0';
	    l = 0;
	    if (setcchar(dst + k, wstr, 0, 0, NULL) != OK) {
		break;
	    }
	    ++k;
	}
	wstr[l++] = wch;
#else
	dst[k++] = (chtype) UChar(src[j]);
#endif
    }
#if USE_WIDEC_SUPPORT
    if (l > 0) {
	wstr[l] = L'\0';
	if (setcchar(dst + k, wstr, 0, 0, NULL) == OK)
	    ++k;
    }
    wstr[0] = L'\0';
    setcchar(dst + k, wstr, 0, 0, NULL);
#else
    dst[k] = 0;
#endif
    return dst;
}

static void
finish(int sig)
{
    endwin();
#if NO_LEAKS
    if (vec_lines != 0) {
	int n;
	for (n = 0; n < num_lines; ++n) {
	    free(vec_lines[n]);
	}
	free(vec_lines);
    }
#endif
    ExitProgram(sig != 0 ? EXIT_FAILURE : EXIT_SUCCESS);
}

static void
show_all(const char *tag)
{
    int i;
    char temp[BUFSIZ];
    NCURSES_CH_T *s;
    time_t this_time;

#if CAN_RESIZE
    _nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
		"%.20s (%3dx%3d) col %d ", tag, LINES, COLS, shift);
    i = (int) strlen(temp);
    if ((i + 7) < (int) sizeof(temp)) {
	_nc_SPRINTF(temp + i, _nc_SLIMIT(sizeof(temp) - (size_t) i)
		    "view %.*s",
		    (int) (sizeof(temp) - 7 - (size_t) i),
		    fname);
    }
#else
    (void) tag;
    _nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
		"view %.*s", (int) sizeof(temp) - 7, fname);
#endif
    move(0, 0);
    printw("%.*s", COLS, temp);
    clrtoeol();
    this_time = time((time_t *) 0);
    _nc_STRNCPY(temp, ctime(&this_time), (size_t) 30);
    if ((i = (int) strlen(temp)) != 0) {
	temp[--i] = 0;
	if (move(0, COLS - i - 2) != ERR)
	    printw("  %s", temp);
    }

    scrollok(stdscr, FALSE);	/* prevent screen from moving */
    for (i = 1; i < LINES; i++) {
	int len;
	int actual = (int) (lptr + i - vec_lines);
	if (actual >= num_lines) {
	    clrtobot();
	    break;
	}
	move(i, 0);
	printw("%3d:", actual);
	clrtoeol();
	if ((s = lptr[i - 1]) == 0) {
	    continue;
	}
	len = ch_len(s);
	if (len > shift) {
#if USE_WIDEC_SUPPORT
	    add_wchstr(s + shift);
#else
	    addchstr(s + shift);
#endif
	}
#if defined(NCURSES_VERSION) || defined(HAVE_WCHGAT)
	if (try_color)
	    wchgat(stdscr, -1, WA_NORMAL, my_pair, NULL);
#endif
    }
    setscrreg(1, LINES - 1);
    scrollok(stdscr, TRUE);
    refresh();
}

#if CAN_RESIZE
/*
 * This uses functions that are "unsafe", but it seems to work on SunOS. 
 * Usually: the "unsafe" refers to the functions that POSIX lists which may be
 * called from a signal handler.  Those do not include buffered I/O, which is
 * used for instance in wrefresh().  To be really portable, you should use the
 * KEY_RESIZE return (which relies on ncurses' sigwinch handler).
 *
 * The 'wrefresh(curscr)' is needed to force the refresh to start from the top
 * of the screen -- some xterms mangle the bitmap while resizing.
 */
static void
adjust(int sig)
{
    if (waiting || sig == 0) {
	struct winsize size;

	if (ioctl(fileno(stdout), TIOCGWINSZ, &size) == 0) {
	    resize_term(size.ws_row, size.ws_col);
	    wrefresh(curscr);
	    show_all(sig ? "SIGWINCH" : "interrupt");
	}
	interrupted = FALSE;
    } else {
	interrupted = TRUE;
    }
    (void) signal(SIGWINCH, adjust);	/* some systems need this */
}
#endif /* CAN_RESIZE */

static void
read_file(const char *filename)
{
    FILE *fp;
    int pass;
    int k;
    size_t j;
    size_t len;
    struct stat sb;
    char *my_blob;
    char **my_vec = 0;

    if (stat(filename, &sb) != 0
	|| (sb.st_mode & S_IFMT) != S_IFREG) {
	failed("input is not a file");
    }

    if (sb.st_size == 0) {
	failed("input is empty");
    }

    if ((fp = fopen(filename, "r")) == 0) {
	failed("cannot open input-file");
    }

    if ((my_blob = malloc((size_t) sb.st_size + 1)) == 0) {
	failed("cannot allocate memory for input-file");
    }

    len = fread(my_blob, sizeof(char), (size_t) sb.st_size, fp);
    my_blob[sb.st_size] = '\0';
    fclose(fp);

    for (pass = 0; pass < 2; ++pass) {
	char *base = my_blob;
	k = 0;
	for (j = 0; j < len; ++j) {
	    if (my_blob[j] == '\n') {
		if (pass) {
		    my_vec[k] = base;
		    my_blob[j] = '\0';
		}
		base = my_blob + j + 1;
		++k;
	    }
	}
	num_lines = k;
	if (base != (my_blob + j))
	    ++num_lines;
	if (!pass &&
	    ((my_vec = typeCalloc(char *, (size_t) k + 2)) == 0)) {
	    failed("cannot allocate line-vector #1");
	}
    }
    if ((vec_lines = typeCalloc(NCURSES_CH_T *, (size_t) num_lines + 2)) == 0)
	failed("cannot allocate line-vector #2");

    Trace(("slurp the file"));
    for (k = 0; k < num_lines; ++k) {
	char *buf = my_vec[k];
	char temp[BUFSIZ], *s, *d;
	int col;

	lptr = &vec_lines[k];

#if USE_WIDEC_SUPPORT
	if (lptr == vec_lines) {
	    if (!memcmp("﻿", buf, 3)) {
		Trace(("trim BOM"));
		s = buf + 3;
		d = buf;
		do {
		} while ((*d++ = *s++) != '\0');
	    }
	}
#endif

	/* convert tabs and nonprinting chars so that shift will work properly */
	for (s = buf, d = temp, col = 0; (*d = *s) != '\0'; s++) {
	    if (*d == '\r') {
		if (s[1] == '\n') {
		    continue;
		} else {
		    break;
		}
	    }
	    if (*d == '\n') {
		*d = '\0';
		break;
	    } else if (*d == '\t') {
		col = (col | 7) + 1;
		while ((d - temp) != col)
		    *d++ = ' ';
	    } else
#if USE_WIDEC_SUPPORT
		col++, d++;
#else
	    if (isprint(UChar(*d))) {
		col++;
		d++;
	    } else {
		_nc_SPRINTF(d, _nc_SLIMIT(sizeof(temp) - (d - buf))
			    "\\%03o", UChar(*s));
		d += strlen(d);
		col = (int) (d - temp);
	    }
#endif
	}
	*lptr = ch_dup(temp);
    }

    free(my_vec);
    free(my_blob);
}

int
main(int argc, char *argv[])
{
    static const char *help[] =
    {
	"Commands:",
	"  q,^Q,ESC       - quit this program",
	"",
	"  p,<Up>         - scroll the viewport up by one row",
	"  n,<Down>       - scroll the viewport down by one row",
	"  l,<Left>       - scroll the viewport left by one column",
	"  r,<Right>      - scroll the viewport right by one column",
	"",
	"  h,<Home>       - scroll the viewport to top of file",
	"  ^F,<PageDn>    - scroll to the next page",
	"  ^B,<PageUp>    - scroll to the previous page",
	"  e,<End>        - scroll the viewport to end of file",
	"",
	"  ^L             - repaint using redrawwin()",
	"",
	"  0 through 9    - enter digits for count",
	"  s              - use entered count for halfdelay() parameter",
	"                 - if no entered count, stop nodelay()",
	"  <space>        - begin nodelay()",
	0
    };

    int i;
    int my_delay = 0;
    NCURSES_CH_T **olptr;
    int value = 0;
    bool done = FALSE;
    bool got_number = FALSE;
    bool ignore_sigs = FALSE;
    bool single_step = FALSE;
#if CAN_RESIZE
    bool nonposix_resize = FALSE;
#endif
    const char *my_label = "Input";

    setlocale(LC_ALL, "");

    while ((i = getopt(argc, argv, "cirstT:")) != -1) {
	switch (i) {
	case 'c':
	    try_color = TRUE;
	    break;
	case 'i':
	    ignore_sigs = TRUE;
	    break;
#if CAN_RESIZE
	case 'r':
	    nonposix_resize = TRUE;
	    break;
#endif
	case 's':
	    single_step = TRUE;
	    break;
#ifdef TRACE
	case 'T':
	    {
		char *next = 0;
		int tvalue = (int) strtol(optarg, &next, 0);
		if (tvalue < 0 || (next != 0 && *next != 0))
		    usage();
		trace((unsigned) tvalue);
	    }
	    break;
	case 't':
	    trace(TRACE_CALLS);
	    break;
#endif
	default:
	    usage();
	}
    }
    if (optind + 1 != argc)
	usage();

    read_file(fname = argv[optind]);

#if CAN_RESIZE
    if (nonposix_resize)
	(void) signal(SIGWINCH, adjust);	/* arrange interrupts to resize */
#endif

    InitAndCatch(initscr(), ignore_sigs ? SIG_IGN : finish);
    keypad(stdscr, TRUE);	/* enable keyboard mapping */
    (void) nonl();		/* tell curses not to do NL->CR/NL on output */
    (void) cbreak();		/* take input chars one at a time, no wait for \n */
    (void) noecho();		/* don't echo input */
    if (!single_step)
	nodelay(stdscr, TRUE);
    idlok(stdscr, TRUE);	/* allow use of insert/delete line */

    if (try_color) {
	if (has_colors()) {
	    start_color();
	    init_pair(my_pair, COLOR_WHITE, COLOR_BLUE);
	    bkgd((chtype) COLOR_PAIR(my_pair));
	} else {
	    try_color = FALSE;
	}
    }

    lptr = vec_lines;
    while (!done) {
	int n, c;

	if (!got_number)
	    show_all(my_label);

	for (;;) {
#if CAN_RESIZE
	    if (interrupted) {
		adjust(0);
		my_label = "interrupt";
	    }
	    waiting = TRUE;
	    c = getch();
	    waiting = FALSE;
#else
	    c = getch();
#endif
	    if ((c < 127) && isdigit(c)) {
		if (!got_number) {
		    MvPrintw(0, 0, "Count: ");
		    clrtoeol();
		}
		addch(UChar(c));
		value = 10 * value + (c - '0');
		got_number = TRUE;
	    } else
		break;
	}
	if (got_number && value) {
	    n = value;
	} else {
	    n = 1;
	}

	if (c != ERR)
	    my_label = keyname(c);
	switch (c) {
	case KEY_DOWN:
	case 'n':
	    olptr = lptr;
	    for (i = 0; i < n; i++)
		if ((lptr - vec_lines) < (num_lines - LINES + 1))
		    lptr++;
		else
		    break;
	    scrl((int) (lptr - olptr));
	    break;

	case KEY_UP:
	case 'p':
	    olptr = lptr;
	    for (i = 0; i < n; i++)
		if (lptr > vec_lines)
		    lptr--;
		else
		    break;
	    scrl((int) (lptr - olptr));
	    break;

	case '<':
	    shift = 0;
	    /* FALLTHRU */
	case 'h':
	    /* FALLTHRU */
	case KEY_HOME:
	    lptr = vec_lines;
	    break;

	case '>':
	    shift = 0;
	    /* FALLTHRU */
	case 'e':
	    /* FALLTHRU */
	case KEY_END:
	    if (num_lines > LINES)
		lptr = (vec_lines + num_lines - LINES + 1);
	    else
		lptr = (vec_lines + (num_lines - 2));
	    break;

	case CTRL('F'):
	    /* FALLTHRU */
	case KEY_NPAGE:
	    if ((lptr - vec_lines) < (num_lines - 5))
		lptr += (LINES - 1);
	    else
		lptr = (vec_lines + num_lines - 2);
	    break;

	case CTRL('B'):
	    /* FALLTHRU */
	case KEY_PPAGE:
	    if ((lptr - vec_lines) >= LINES)
		lptr -= (LINES - 1);
	    else
		lptr = vec_lines;
	    break;

	case 'r':
	case KEY_RIGHT:
	    shift += n;
	    break;

	case 'l':
	case KEY_LEFT:
	    shift -= n;
	    if (shift < 0) {
		shift = 0;
		beep();
	    }
	    break;

	case 'q':
	case QUIT:
	case ESCAPE:
	    done = TRUE;
	    break;

#ifdef KEY_RESIZE
	case KEY_RESIZE:	/* ignore this; ncurses will repaint */
	    break;
#endif
	case 's':
#if HAVE_HALFDELAY
	    if (got_number) {
		halfdelay(my_delay = n);
	    } else {
		nodelay(stdscr, FALSE);
		my_delay = -1;
	    }
#else
	    nodelay(stdscr, FALSE);
	    my_delay = -1;
#endif
	    break;
	case ' ':
	    nodelay(stdscr, TRUE);
	    my_delay = 0;
	    break;
	case CTRL('L'):
	    redrawwin(stdscr);
	    break;
	case ERR:
	    if (!my_delay)
		napms(50);
	    break;
	case HELP_KEY_1:
	    popup_msg(stdscr, help);
	    break;
	default:
	    beep();
	    break;
	}
	if (c >= KEY_MIN || (c > 0 && !isdigit(c))) {
	    got_number = FALSE;
	    value = 0;
	}
    }

    finish(0);			/* we're done */
}
