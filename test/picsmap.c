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
 * $Id: picsmap.c,v 1.17 2017/05/21 00:22:22 tom Exp $
 *
 * Author: Thomas E. Dickey
 *
 * A little more interesting than "dots", read a simple image into memory and
 * measure the time taken to paint it normally vs randomly.
 *
 * TODO handle hex color-codes for xpm other than 3-bytes
 * TODO read rgb.txt to handle xpm color names other than "None"
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

static void
free_data(char **data)
{
    free(data[0]);
    free(data);
}

static void
free_pics_head(PICS_HEAD * pics)
{
    free(pics->pairs);
    free(pics->cells);
    free(pics);
}

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

static int match_c(const char *, const char *,...) GCC_SCANFLIKE(2,3);

static const char *
skip_s(const char *s)
{
    while (isspace(UChar(*s)))
	s++;
    return s;
}

static int
match_c(const char *source, const char *pattern,...)
{
    const char *last_s = source + strlen(source);
    va_list ap;
    int ch;
    int *ip;
    char *cp;
    long lv;

    va_start(ap, pattern);

    while (*pattern != '\0') {
	ch = UChar(*pattern++);
	/* blank in the pattern matches zero-or-more blanks in source */
	if (isspace(ch)) {
	    source = skip_s(source);
	    continue;
	}
	/* %c, %d, %s are like sscanf except for special treatment of blanks */
	if (ch == '%' && *pattern != '\0' && strchr("cds", *pattern)) {
	    bool found = FALSE;
	    ch = *pattern++;
	    switch (ch) {
	    case 'c':
		cp = va_arg(ap, char *);
		*cp = *source++;
		break;
	    case 'd':
		ip = va_arg(ap, int *);
		lv = strtol(source, &cp, 0);
		if (cp != 0 && cp != source) {
		    *ip = (int) lv;
		    source = cp;
		} else {
		    goto finish;
		}
		break;
	    case 's':
		cp = va_arg(ap, char *);
		while (*source != '\0') {
		    ch = UChar(*source);
		    if (isspace(ch)) {
			break;
		    } else if (found && (ch == *skip_s(pattern))) {
			break;
		    } else {
			*cp++ = *source++;
			found = TRUE;
		    }
		}
		*cp = '\0';
		break;
	    }
	    continue;
	}
	/* other characters are matched literally */
	if (*source++ != ch) {
	    break;
	}
    }
  finish:

    va_end(ap);
    if (source > last_s)
	source = last_s;
    return (*source || *pattern) ? 0 : 1;
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

static PICS_HEAD *
parse_xpm(char **data)
{
    int state = 0;
    PICS_HEAD *result = typeCalloc(PICS_HEAD, 1);
    int n;
    int cells = 0;
    int color = 0;
    int cpp = 1;		/* chars per pixel */
    int num[6];
    int which = 0;
    char ch;
    const char *cs;
    char *s;
    char buf[BUFSIZ];
    char arg1[BUFSIZ];
    char arg2[BUFSIZ];
    char arg3[BUFSIZ];
    char **list = 0;

    for (n = 0; data[n] != 0; ++n) {
	if (strlen(s = data[n]) >= sizeof(buf) - 1)
	    continue;
	switch (state) {
	case 0:
	    if (match_c(s, " /* XPM */ ")) {
		state = 1;
	    }
	    break;
	case 1:
	    if (match_c(s, " static char * %s [] = %c ", arg1, &ch) &&
		ch == L_CURL) {
		result->name = strdup(arg1);
		state = 2;
	    }
	    break;
	case 2:
	    if (match_c(s, " \" %d %d %d %d \" , ",
			num + 0, num + 1, num + 2, num + 3) ||
		match_c(s, " \" %d %d %d %d %d %d \" , ",
			num + 0, num + 1, num + 2, num + 3, num + 4, num + 5)) {
		result->wide = num[0];
		result->high = num[1];
		result->colors = num[2];
		result->pairs = typeCalloc(PICS_PAIR, result->colors);
		cells = (size_t) (result->wide * result->high);
		result->cells = typeCalloc(PICS_CELL, cells);
		list = typeCalloc(char *, result->colors);
		cpp = num[3];
		state = 3;
	    }
	    break;
	case 3:
	    if (match_c(s, " \" %s %s %s \" , ", arg1, arg2, arg3)) {
		;
	    } else if (match_c(s, " \" %s %s \" , ", arg2, arg3)) {
		strcpy(arg1, " ");
	    } else {
		break;
	    }
	    while ((int) strlen(arg1) < cpp)
		strcat(arg1, " ");
	    list[color] = strdup(arg1);
	    if (!strcmp(arg3, "None")) {
		result->pairs[color].fg = -1;
	    } else if (*arg3 == '#') {
		unsigned long value = strtoul(arg3 + 1, &s, 16);
		result->pairs[color].fg = (int) value;
	    } else {
		result->pairs[color].fg = 0;	/* actually an error */
	    }
	    if (++color >= result->colors)
		state = 4;
	    break;
	case 4:
	    if (*(cs = skip_s(s)) == '"') {
		++cs;
		while (*cs != '\0' && *cs != '"') {
		    int c;

		    for (c = 0; c < result->colors; ++c) {
			if (!strncmp(cs, list[c], cpp)) {
			    result->cells[which].ch = list[c][0];
			    result->cells[which].fg = c;
			    break;
			}
		    }

		    if (result->cells[which].ch == 0) {
			result->cells[which].ch = '?';
			result->cells[which].fg = 0;
		    }

		    if (++which >= cells) {
			state = 5;
			break;
		    }
		    for (c = cpp; c > 0; --c, ++cs) ;
		}
	    }
	    break;
	}
    }

    if (result && list) {
	for (n = 0; n < result->colors; ++n)
	    free(list[n]);
	free(list);
    }

    if (state < 5) {
	free_pics_head(result);
	result = 0;
    }

    return result;
}

static PICS_HEAD *
read_picture(const char *filename, char **data)
{
    PICS_HEAD *pics;
    if ((pics = parse_xbm(data)) == 0) {
	if ((pics = parse_xpm(data)) == 0) {
	    free_data(data);
	    giveup("unexpected file-format for \"%s\"", filename);
	}
    }
    return pics;
}

static void
dump_picture(PICS_HEAD * pics)
{
    int y, x;

    printf("Name %s\n", pics->name);
    printf("Size %dx%d\n", pics->high, pics->wide);
    printf("Color\n");
    for (y = 0; y < pics->colors; ++y) {
	if (pics->pairs[y].fg < 0) {
	    printf(" %3d: %d\n", y, pics->pairs[y].fg);
	} else {
	    printf(" %3d: #%06x\n", y, pics->pairs[y].fg);
	}
    }
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
	    }
	    if ((pics = read_picture(argv[n], data)) == 0) {
		free_data(data);
		giveup("unexpected file-format for \"%s\"", argv[n]);
	    }
	    if (isatty(fileno(stdout))) {
		show_picture(pics);
	    } else {
		dump_picture(pics);
	    }
	    free_data(data);
	    free_pics_head(pics);
	}
    } else {
	usage();
    }
    ExitProgram(EXIT_SUCCESS);
}
