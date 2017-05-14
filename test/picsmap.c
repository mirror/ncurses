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
 * $Id: picsmap.c,v 1.8 2017/05/14 01:26:30 tom Exp $
 *
 * Author: Thomas E. Dickey
 *
 * A little more interesting than "dots", read a simple image into memory and
 * measure the time taken to paint it normally vs randomly.
 *
 * TODO read xpm file
 * TODO read "convert" via pipe (from ImageMagick)
 * TODO write cells/second to stderr (or log)
 * TODO write picture left-to-right/top-to-bottom
 * TODO write picture randomly
 * TODO add one-shot option vs repeat-count before exiting
 * TODO add option for assumed palette of terminal
 * TODO add option for init_color
 * TODO use pad to allow pictures larger than screen
 */
#include <test.priv.h>

#include <sys/types.h>
#include <sys/stat.h>

#define  L_CURL '{'
#define  R_CURL '}'

typedef struct {
    int ch;			/* nominal character to display */
    int fg;			/* foreground color */
} PICS_CELL;

typedef struct {
    int fg;
    int bg;
} PICS_PAIR;

typedef struct {
    char *name;
    int high;
    int wide;
    int colors;
    PICS_PAIR *pairs;
    PICS_CELL *cells;
} PICS_HEAD;

static bool in_curses = FALSE;

/*
 * Simplify reading xbm/xpm files by first making an array of lines.  Blank
 * lines are filtered out.
 */
static char **
read_file(const char *filename)
{
    char **result = 0;
    struct stat sb;

    printf("** %s\n", filename);
    if (stat(filename, &sb) == 0
	&& (sb.st_mode & S_IFMT) == S_IFREG
	&& sb.st_size != 0) {
	size_t size = (size_t) sb.st_size;
	char *blob = typeMalloc(char, size + 1);
	bool had_line = TRUE;
	unsigned j;
	unsigned k = 0;

	result = typeCalloc(char *, size + 1);
	if (blob != 0 && result != 0) {
	    FILE *fp = fopen(filename, "r");
	    if (fp != 0) {
		if (fread(blob, sizeof(char), size, fp) == size) {
		    for (j = 0; (size_t) j < size; ++j) {
			if (blob[j] == '\n') {
			    blob[j] = '\0';
			    had_line = TRUE;
			} else if (had_line) {
			    had_line = FALSE;
			    result[k++] = blob + j;
			}
		    }
		    result[k] = 0;
		}
		fclose(fp);
	    }
	}
	if (k == 0) {
	    free(blob);
	    free(result);
	    result = 0;
	}
    }
    return result;
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: picsmap [xbm-file [...]]"
    };
    size_t n;

    fflush(stdout);
    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);
    ExitProgram(EXIT_FAILURE);
}

static void
giveup(const char *fmt,...)
{
    va_list ap;
    if (in_curses)
	endwin();
    fflush(stdout);
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fputc('\n', stderr);
    va_end(ap);
    usage();
}

static int
map_color(int value)
{
    int r = (value & 0xff0000) >> 16;
    int g = (value & 0x00ff00) >> 8;
    int b = (value & 0x0000ff) >> 0;
    /* TODO simple mapping into COLOR_BLACK .. COLOR_WHITE */
    int result = ((r >= 128) << 2) + ((g >= 128) << 1) + (b >= 128);
    return result;
}

static int
bytes_of(int value)
{
    if (value & 7) {
	value |= 7;
	value++;
    }
    return value;
}

static PICS_HEAD *
parse_xbm(char **data)
{
    int n;
    int state = 0;
    char buf[BUFSIZ];
    int num;
    char ch;
    char *s;
    char *t;
    PICS_HEAD *result = typeCalloc(PICS_HEAD, 1);
    size_t which = 0;
    size_t cells = 0;

    for (n = 0; data[n] != 0; ++n) {
	if (strlen(s = data[n]) >= sizeof(buf) - 1)
	    continue;
	switch (state) {
	case 0:
	case 1:
	case 2:
	    if (sscanf(s, "#define %s %d%c", buf, &num, &ch) >= 2) {
		if ((t = strstr(buf, "_width")) != 0) {
		    state |= 1;
		    result->wide = bytes_of(num);
		} else if ((t = strstr(buf, "_height")) != 0) {
		    state |= 2;
		    result->high = num;
		}
		*t = '\0';
		if (result->name) {
		    if (strcmp(result->name, buf)) {
			goto finish;
		    }
		} else {
		    result->name = strdup(buf);
		}
	    }
	    break;
	case 3:
	    if (sscanf(s, "static char %[^_ ]_bits[]%c", buf, &ch) >= 1) {
		if (strcmp(result->name, buf)) {
		    goto finish;
		}
		state = 4;
		cells = (size_t) (result->wide * result->high);
		result->cells = typeCalloc(PICS_CELL, cells);
		if ((s = strchr(s, L_CURL)) == 0)
		    break;
		++s;
	    } else {
		break;
	    }
	case 4:
	    while (*s != '\0') {
		while (isspace(UChar(*s))) {
		    ++s;
		}
		if (isdigit(UChar(*s))) {
		    long value = strtol(s, &t, 0);
		    int b;
		    if (t != s || value > 255 || value < 0) {
			s = t;
		    } else {
			state = -1;
			goto finish;
		    }
		    /* TODO: which order? */
		    for (b = 0; b < 8; ++b) {
			if (((1L << b) & value) != 0) {
			    result->cells[which].ch = '*';
			    result->cells[which].fg = 1;
			} else {
			    result->cells[which].ch = ' ';
			    result->cells[which].fg = 0;
			}
			if (++which > cells) {
			    state = -1;
			    goto finish;
			}
		    }
		}
		if (*s == R_CURL) {
		    state = 5;
		    goto finish;
		} else if (*s == ',') {
		    ++s;
		}
	    }
	    break;
	default:
	    break;
	}
    }
  finish:
    if (state < 4) {
	if (result) {
	    free(result->pairs);
	    free(result->cells);
	    free(result);
	    result = 0;
	}
    } else {
	result->colors = 2;
	result->pairs = typeCalloc(PICS_PAIR, 2);
	result->pairs[1].fg = 0xffffff;
    }
    return result;
}

static void
dump_picture(PICS_HEAD * pics)
{
    int y, x;

    printf("Name %s\n", pics->name);
    printf("Size %dx%d\n", pics->high, pics->wide);
    for (y = 0; y < pics->high; ++y) {
	for (x = 0; x < pics->wide; ++x) {
	    putchar(pics->cells[y * pics->wide + x].ch);
	}
	putchar('\n');
    }
}

static void
show_picture(PICS_HEAD * pics)
{
    int y, x;
    int n;

    if (!in_curses) {
	in_curses = TRUE;
	initscr();
	cbreak();
	noecho();
	if (has_colors())
	    start_color();
    }
    scrollok(stdscr, FALSE);
    if (has_colors()) {
	for (n = 0; n < pics->colors; ++n) {
	    init_pair((short) (n + 1),
		      (short) map_color(pics->pairs[n].fg),
		      COLOR_BLACK);
	}
	attrset(COLOR_PAIR(1));
	erase();
    }
    for (y = 0; y < pics->high; ++y) {
	if (y >= LINES)
	    break;
	move(y, 0);
	for (x = 0; x < pics->wide; ++x) {
	    if (x >= COLS)
		break;
	    n = (y * pics->wide + x);
	    attrset(COLOR_PAIR(pics->cells[n].fg + 1));
	    addch((chtype) pics->cells[n].ch);
	}
    }
    mvgetch(0, 0);
    endwin();
}

int
main(int argc, char *argv[])
{
    int n;

    if (argc > 1) {
	for (n = 1; n < argc; ++n) {
	    char **data = read_file(argv[n]);
	    PICS_HEAD *pics;
	    if (data == 0) {
		giveup("cannot read \"%s\"", argv[n]);
	    } else if ((pics = parse_xbm(data)) == 0) {
		giveup("unexpected file-format for \"%s\"", argv[n]);
	    } else if (isatty(fileno(stdout))) {
		show_picture(pics);
	    } else {
		dump_picture(pics);
	    }
	}
    } else {
	usage();
    }
    ExitProgram(EXIT_SUCCESS);
}
