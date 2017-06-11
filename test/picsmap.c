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
 * $Id: picsmap.c,v 1.50 2017/06/11 00:37:27 tom Exp $
 *
 * Author: Thomas E. Dickey
 *
 * A little more interesting than "dots", read a simple image into memory and
 * measure the time taken to paint it normally vs randomly.
 *
 * TODO write cells/second to stderr (or log)
 * TODO write picture left-to-right/top-to-bottom
 * TODO write picture randomly
 * TODO add one-shot option vs repeat-count before exiting
 * TODO add option "-xc" for init_color vs init_extended_color
 * TODO add option "-xa" for init_pair vs alloc_pair
 * TODO use pad to allow pictures larger than screen
 * TODO improve load of image-file's color-table using tsearch.
 * TODO add option to just use convert (which can scale) vs builtin xbm/xpm.
 */
#include <test.priv.h>

#include <sys/types.h>
#include <sys/stat.h>

#undef CUR			/* use only the curses interface */

#define  L_BLOCK '['
#define  R_BLOCK ']'

#define  L_CURLY '{'
#define  R_CURLY '}'

#define okCOLOR(n) ((n) >= 0 && (n) < COLORS)
#define okRGB(n)   ((n) >= 0 && (n) <= 1000)
#define Scaled256(n) (NCURSES_COLOR_T) (int)(((n) * 1000.0) / 256)
#define ScaledColor(n) (NCURSES_COLOR_T) (int)(((n) * 1000.0) / scale)

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

typedef struct {
    const char *name;
    int value;
} RGB_NAME;

typedef struct {
    short red;
    short green;
    short blue;
} RGB_DATA;

static void giveup(const char *fmt,...) GCC_PRINTFLIKE(1, 2);

static bool in_curses = FALSE;
static RGB_NAME *rgb_table;
static RGB_DATA *all_colors;

#if HAVE_ALLOC_PAIR && HAVE_INIT_EXTENDED_COLOR
#define USE_EXTENDED_COLORS 1
static bool use_extended_pairs = FALSE;
static bool use_extended_colors = FALSE;
#else
#define USE_EXTENDED_COLORS 0
#endif

static void
free_data(char **data)
{
    if (data != 0) {
	free(data[0]);
	free(data);
    }
}

static void
free_pics_head(PICS_HEAD * pics)
{
    if (pics != 0) {
	free(pics->pairs);
	free(pics->cells);
	free(pics);
    }
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

    if (in_curses)
	endwin();

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
	"Usage: picsmap [options] [imagefile [...]]"
	,"Read/display one or more xbm/xpm files (possibly use \"convert\")"
	,""
	,"Options:"
	,"  -p palette"
	,"  -r rgb-path"
#if USE_EXTENDED_COLORS
	,"  -x [p]   use extension (p=init_extended_pair)"
#endif
    };
    size_t n;

    if (in_curses)
	endwin();

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

static void
init_palette(const char *palette_file)
{
    if (palette_file != 0) {
	char **data = read_file(palette_file);
	int cp;

	all_colors = typeMalloc(RGB_DATA, (unsigned) COLORS);
	for (cp = 0; cp < COLORS; ++cp) {
	    color_content((short) cp,
			  &all_colors[cp].red,
			  &all_colors[cp].green,
			  &all_colors[cp].blue);
	}
	if (palette_file != 0 && data != 0) {
	    int n;
	    int red, green, blue;
	    int scale = 1000;
	    int c;
	    for (n = 0; data[n] != 0; ++n) {
		if (sscanf(data[n], "scale:%d", &c) == 1) {
		    scale = c;
		} else if (sscanf(data[n], "%d:%d %d %d",
				  &c,
				  &red,
				  &green,
				  &blue) == 4
			   && okCOLOR(c)
			   && okRGB(red)
			   && okRGB(green)
			   && okRGB(blue)) {
		    /* *INDENT-EQLS* */
		    all_colors[c].red   = ScaledColor(red);
		    all_colors[c].green = ScaledColor(green);
		    all_colors[c].blue  = ScaledColor(blue);
		}
	    }
	}
	free_data(data);
    } else if (COLORS   > 1) {
	/* *INDENT-EQLS* */
	int power2 = 1;
	int shift = 0;

	while (power2 < COLORS) {
	    ++shift;
	    power2 <<= 1;
	}

	if ((power2 != COLORS) || ((shift % 3) != 0)) {
	    giveup("With %d colors, you need a palette-file", COLORS);
	}
    }
}

/*
 * Map the 24-bit RGB value to a color index if using a palette, otherwise to a
 * direct color value.
 */
static int
map_color(int value)
{
    int result = value;

    if (result < 0) {
	result = -1;
    } else {
	/* *INDENT-EQLS* */
	int red   = (value & 0xff0000) >> 16;
	int green = (value & 0x00ff00) >> 8;
	int blue  = (value & 0x0000ff) >> 0;

	if (all_colors != 0) {
#define Diff2(n,m) ((m) - all_colors[n].m) * ((m) - all_colors[n].m)
#define Diff2S(n) Diff2(n,red) + Diff2(n,green) + Diff2(n,blue)
	    int d2 = Diff2S(0);
	    int n;

	    /* *INDENT-EQLS* */
	    red   = Scaled256(red);
	    green = Scaled256(green);
	    blue  = Scaled256(blue);

	    for (result = 0, n = 1; n < COLORS; ++n) {
		int d = Diff2(n, red) + Diff2(n, green) + Diff2(n, blue);
		if (d < d2) {
		    d2 = d;
		    result = n;
		}
	    }
	} else {		/* direct color */
	    int power2 = 1;
	    int shifts = 8;

	    while (power2 < COLORS) {
		power2 <<= 3;
		shifts--;
	    }

	    if (shifts > 0) {
		/* TODO: round up */
		red >>= shifts;
		green >>= shifts;
		blue >>= shifts;
		result = ((red << (2 * (8 - shifts)))
			  + (green << (8 - shifts))
			  + blue);
	    }
	}
    }
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

static char *
skip_s(char *s)
{
    while (isspace(UChar(*s)))
	s++;
    return s;
}

static const char *
skip_cs(const char *s)
{
    while (isspace(UChar(*s)))
	s++;
    return s;
}

static char *
skip_word(char *s)
{
    s = skip_s(s);
    while (isgraph(UChar(*s)))
	s++;
    return s;
}

static int
match_c(const char *source, const char *pattern,...)
{
    int limit = (int) strlen(source);
    const char *last_s = source + limit;
    va_list ap;
    int ch;
    int *ip;
    char *cp;
    long lv;

    va_start(ap, pattern);

    limit = -1;
    while (*pattern != '\0') {
	ch = UChar(*pattern++);
	/* blank in the pattern matches zero-or-more blanks in source */
	if (isspace(ch)) {
	    source = skip_cs(source);
	    continue;
	}
	/* %c, %d, %s are like sscanf except for special treatment of blanks */
	if (ch == '%' && *pattern != '\0' && strchr("cdnsx", *pattern)) {
	    bool found = FALSE;
	    ch = *pattern++;
	    switch (ch) {
	    case 'c':
		cp = va_arg(ap, char *);
		do {
		    *cp++ = *source++;
		} while (--limit > 0);
		break;
	    case 'd':
	    case 'x':
		limit = -1;
		ip = va_arg(ap, int *);
		lv = strtol(source, &cp, ch == 'd' ? 10 : 16);
		if (cp != 0 && cp != source) {
		    *ip = (int) lv;
		    source = cp;
		} else {
		    goto finish;
		}
		break;
	    case 'n':
		/* not really sscanf... */
		limit = *va_arg(ap, int *);
		break;
	    case 's':
		limit = -1;
		cp = va_arg(ap, char *);
		while (*source != '\0') {
		    ch = UChar(*source);
		    if (isspace(ch)) {
			break;
		    } else if (found && (ch == *skip_cs(pattern))) {
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

static int
match_colors(const char *source, int cpp, char *arg1, char *arg2, char *arg3)
{
    int result = 0;

    /* most files use a quasi-fixed format */
    if (match_c(source, " \"%n%c %s %s \" , ", &cpp, arg1, arg2, arg3)) {
	arg1[cpp] = '\0';
	result = 1;
    } else {
	char *t;
	const char *s = skip_cs(source);
	size_t have = strlen(source);

	if (*s++ == '"' && have > ((size_t) cpp + 2)) {
	    memcpy(arg1, s, (size_t) cpp);
	    s += cpp;
	    while (*s++ == '\t') {
		for (t = arg2; (*s != '\0') && strchr("\t\"", *s) == 0;) {
		    if (*s == ' ') {
			s = skip_cs(s);
			break;
		    }
		    *t++ = *s++;
		    *t = '\0';
		}
		for (t = arg3; (*s != '\0') && strchr("\t\"", *s) == 0;) {
		    *t++ = *s++;
		    *t = '\0';
		}
		if (!strcmp(arg2, "c")) {
		    result = 1;
		    break;
		}
	    }
	}
    }
    return result;
}

static RGB_NAME *
parse_rgb(char **data)
{
    char buf[BUFSIZ];
    int n;
    unsigned long r, g, b;
    char *s, *t;
    size_t item = 0;
    size_t need;
    RGB_NAME *result = 0;

    for (need = 0; data[need] != 0; ++need) ;
    result = typeCalloc(RGB_NAME, need + 2);

    for (n = 0; data[n] != 0; ++n) {
	if (strlen(t = data[n]) >= sizeof(buf) - 1)
	    continue;
	if (*(s = skip_s(t)) == '!')
	    continue;

	r = strtoul(s, &t, 10);
	s = skip_s(t);
	g = strtoul(s, &t, 10);
	s = skip_s(t);
	b = strtoul(s, &t, 10);
	s = skip_s(t);

	result[item].name = s;
	t = s + strlen(s);
	while (t-- != s && isspace(UChar(*t))) {
	    *t = '\0';
	}
	result[item].value = (int) ((r & 0xff) << 16 | (g & 0xff) << 8 | (b
									  & 0xff));
	++item;
    }

    result[item].name = "none";
    result[item].value = -1;

    return result;
}

static RGB_NAME *
lookup_rgb(const char *name)
{
    RGB_NAME *result = 0;
    if (rgb_table != 0) {
	int n;
	for (n = 0; rgb_table[n].name != 0; ++n) {
	    if (!strcasecmp(name, rgb_table[n].name)) {
		result = &rgb_table[n];
		break;
	    }
	}
    }
    return result;
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
		if ((s = strchr(s, L_CURLY)) == 0)
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
		if (*s == R_CURLY) {
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
	    free_pics_head(result);
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
    RGB_NAME *by_name;
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
		ch == L_CURLY) {
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
		cells = (result->wide * result->high);
		result->cells = typeCalloc(PICS_CELL, cells);
		list = typeCalloc(char *, result->colors);
		cpp = num[3];
		state = 3;
	    }
	    break;
	case 3:
	    if (!match_colors(s, cpp, arg1, arg2, arg3)) {
		break;
	    }
	    list[color] = strdup(arg1);
	    if ((by_name = lookup_rgb(arg3)) != 0) {
		result->pairs[color].fg = by_name->value;
	    } else if (*arg3 == '#') {
		char *rgb = arg3 + 1;
		unsigned long value = strtoul(rgb, &s, 16);
		switch ((int) strlen(rgb)) {
		case 6:
		    break;
		case 12:
		    value = (((value >> 24) & 0xff0000L)
			     | ((value >> 16) & 0xff00L)
			     | ((value >> 8) & 0xffL));
		    break;
		default:
		    printf("unexpected rgb value %s\n", rgb);
		    break;
		}
		result->pairs[color].fg = (int) value;
	    } else {
		result->pairs[color].fg = 0;	/* actually an error */
	    }
	    if (++color >= result->colors)
		state = 4;
	    break;
	case 4:
	    if (*(cs = skip_cs(s)) == '"') {
		++cs;
		while (*cs != '\0' && *cs != '"') {
		    int c;

		    for (c = 0; c < result->colors; ++c) {
			if (!strncmp(cs, list[c], (size_t) cpp)) {
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

/*
 * The obscurely-named "convert" is provided by ImageMagick
 */
static PICS_HEAD *
parse_img(const char *filename)
{
    char *cmd = malloc(strlen(filename) + 256);
    FILE *pp;
    char buffer[BUFSIZ];
    bool failed = FALSE;
    PICS_HEAD *result = typeCalloc(PICS_HEAD, 1);
    int pic_x = 0;
    int pic_y = 0;
    int width = in_curses ? COLS : 80;

    sprintf(cmd, "identify \"%s\"", filename);

    if ((pp = popen(cmd, "r")) != 0) {
	if (fgets(buffer, sizeof(buffer), pp) != 0) {
	    size_t n = strlen(filename);
	    if (strlen(buffer) > n &&
		!strncmp(buffer, filename, n) &&
		isspace(UChar(buffer[n])) &&
		sscanf(skip_word(buffer + n), " %dx%d ", &pic_x, &pic_y) == 2) {
		/* distort image to make it show normally on terminal */
		pic_x = (166 * pic_x) / 100;
	    } else {
		pic_x = pic_y = 0;
	    }
	}
	pclose(pp);
    }
    if (pic_x <= 0 || pic_y <= 0)
	goto finish;

    sprintf(cmd, "convert -resize %dx%d\\! -thumbnail %dx \"%s\" txt:-",
	    pic_x, pic_y, width, filename);

    if ((pp = popen(cmd, "r")) != 0) {
	int count = 0;
	int col = 0;
	int row = 0;
	int len = 0;
	while (fgets(buffer, sizeof(buffer), pp) != 0) {
	    if (strlen(buffer) > 160) {		/* 80 columns would be enough */
		failed = TRUE;
		break;
	    }
	    if (count++ == 0) {
		if (match_c(buffer,
			    "# ImageMagick pixel enumeration: %d,%d,%d,srgba ",
			    &col, &row, &len)) {
		    result->name = strdup(filename);
		    result->wide = col;
		    result->high = row;
		    result->colors = 256;
		    result->pairs = typeCalloc(PICS_PAIR, result->colors);
		    result->cells = typeCalloc(PICS_CELL, (size_t) (col * row));
		} else {
		    failed = TRUE;
		    break;
		}
	    } else {
		/* subsequent lines begin "col,row: (r,g,b,a) #RGB" */
		int r, g, b;
		unsigned check;
		int which, c;
		char *s = strchr(buffer, '#');
		if (s != 0) {
		    /* after the "#RGB", there are differences - just ignore */
		    while (*s != '\0' && !isspace(UChar(*s)))
			++s;
		    *++s = '\0';
		}
		if (match_c(buffer,
			    "%d,%d: (%d,%d,%d,255) #%x ",
			    &col, &row,
			    &r, &g, &b,
			    &check)) {
		    if (r > 255 ||
			g > 255 ||
			b > 255 ||
			check != (unsigned) ((r << 16) | (g << 8) | b)) {
			failed = TRUE;
			break;
		    }
		    for (c = 0; c < result->colors; ++c) {
			if (result->pairs[c].fg == (int) check) {
			    break;
			} else if (result->pairs[c].fg == 0) {
			    result->pairs[c].fg = (int) check;
			    break;
			}
		    }
		    if (c >= result->colors) {
			int more = (result->colors * 3) / 2;
			PICS_PAIR *p = typeRealloc(PICS_PAIR, more, result->pairs);
			if (p != 0) {
			    result->colors = more;
			    result->pairs = p;
			    result->pairs[c].fg = (int) check;
			    result->pairs[c].bg = 0;
			    while (++c < more) {
				result->pairs[c].fg = 0;
				result->pairs[c].bg = 0;
			    }
			}
		    }
		    which = col + (row * result->wide);
		    result->cells[which].ch = ((in_curses ||
						check == 0xffffff)
					       ? ' '
					       : '#');
		    result->cells[which].fg = (c < result->colors) ? c : -1;
		} else {
		    failed = TRUE;
		    break;
		}
	    }
	}
	pclose(pp);
	if (!failed) {
	    for (len = result->colors; len > 3; len--) {
		if (result->pairs[len - 1].fg == 0) {
		    result->colors = len - 1;
		} else {
		    break;
		}
	    }
	}
    }
  finish:
    free(cmd);

    if (failed) {
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
	    if ((pics = parse_img(filename)) == 0) {
		free_data(data);
		giveup("unexpected file-format for \"%s\"", filename);
	    }
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
    int my_pair, my_color;

    if (has_colors()) {
	for (n = 0; n < pics->colors; ++n) {
	    my_pair = (n + 1);
	    my_color = map_color(pics->pairs[n].fg);
#if USE_EXTENDED_COLORS
	    if (use_extended_pairs) {
		init_extended_pair(my_pair, my_color, my_color);
	    } else
#endif
	    {
		my_pair &= 0x7fff;
		my_color &= 0x7fff;
		init_pair((short) my_pair, (short) my_color, (short) my_color);
	    }
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
	    my_pair = pics->cells[n].fg + 1;
#if USE_EXTENDED_COLORS
	    if (use_extended_pairs) {
		cchar_t temp;
		wchar_t wch[2];
		wch[0] = (wchar_t) pics->cells[n].ch;
		wch[1] = 0;
		setcchar(&temp, wch, A_NORMAL, (short) my_pair, &my_pair);
		add_wch(&temp);
	    } else
#endif
	    {
		attrset(COLOR_PAIR(my_pair));
		addch((chtype) pics->cells[n].ch);
	    }
	}
    }
    mvgetch(0, 0);
    endwin();
}

int
main(int argc, char *argv[])
{
    int n;
    const char *palette_path = 0;
    const char *rgb_path = "/etc/X11/rgb.txt";

    while ((n = getopt(argc, argv, "p:r:x:")) != -1) {
	switch (n) {
	case 'p':
	    palette_path = optarg;
	    break;
	case 'r':
	    rgb_path = optarg;
	    break;
#if USE_EXTENDED_COLORS
	case 'x':
	    {
		char *s = optarg;
		while (*s) {
		    switch (*s++) {
		    case 'p':
			use_extended_pairs = TRUE;
			break;
		    case 'c':
			use_extended_colors = TRUE;
			break;
		    default:
			usage();
			break;
		    }
		}
	    }
	    break;
#endif
	default:
	    usage();
	    break;
	}
    }

    if (optind < argc) {
	char **rgb_data = read_file(rgb_path);

	if (rgb_data)
	    rgb_table = parse_rgb(rgb_data);

	if (isatty(fileno(stdout))) {
	    in_curses = TRUE;
	    initscr();
	    cbreak();
	    noecho();
	    if (has_colors()) {
		start_color();
		init_palette(palette_path);
	    }
	    scrollok(stdscr, FALSE);
	    endwin();
	}
	if (optind >= argc)
	    giveup("expected at least one image filename");

	for (n = optind; n < argc; ++n) {
	    PICS_HEAD *pics;
	    char **data = read_file(argv[n]);

	    if (data == 0) {
		giveup("cannot read \"%s\"", argv[n]);
	    }
	    pics = read_picture(argv[n], data);
	    if (in_curses) {
		show_picture(pics);
	    } else {
		dump_picture(pics);
	    }
	    free_data(data);
	    free_pics_head(pics);
	}
	free_data(rgb_data);
	free(rgb_table);
    } else {
	usage();
    }
    ExitProgram(EXIT_SUCCESS);
}
