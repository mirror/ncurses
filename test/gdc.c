/*
 * Grand digital clock for curses compatible terminals
 * Usage: gdc [-s] [n]   -- run for n seconds (default infinity)
 * Flags: -s: scroll
 *
 * modified 10-18-89 for curses (jrl)
 * 10-18-89 added signal handling
 *
 * $Id: gdc.c,v 1.8 1997/01/19 00:59:28 tom Exp $
 */

#include <test.priv.h>

#include <time.h>
#include <signal.h>
#include <string.h>

#define YBASE	10
#define XBASE	10
#define XLENGTH	54
#define YDEPTH	5

/* it won't be */
static time_t now; /* yeah! */
static struct tm *tm;

static short disp[11] = {
	075557, 011111, 071747, 071717, 055711,
	074717, 074757, 071111, 075757, 075717, 002020
};
static long old[6], next[6], new[6], mask;
static char scrol;

static int sigtermed = 0;

static int hascolor = 0;

static void set(int, int);
static void standt(int);
static void movto(int, int);

static
RETSIGTYPE sighndl(int signo)
{
	signal(signo, sighndl);
	sigtermed=signo;
}

int
main(int argc, char *argv[])
{
long t, a;
int i, j, s, k;
int n = 0;

	signal(SIGINT,sighndl);
	signal(SIGTERM,sighndl);
	signal(SIGKILL,sighndl);

	initscr();
	cbreak();
	noecho();
	nodelay(stdscr, 1);

	hascolor = has_colors();

	if(hascolor) {
		int bg = COLOR_BLACK;
		start_color();
#ifdef NCURSES_VERSION
		if (use_default_colors() == OK)
			bg = -1;
#endif
		init_pair(1, COLOR_BLACK, COLOR_RED);
		init_pair(2, COLOR_RED,   bg);
		init_pair(3, COLOR_WHITE, bg);
		attrset(COLOR_PAIR(2));
	}

	clear();
	refresh();
	while(--argc > 0) {
		if(**++argv == '-')
			scrol = 1;
		else
			n = atoi(*argv);
	}

	if(hascolor) {
		attrset(COLOR_PAIR(3));

		mvaddch(YBASE - 1,  XBASE - 1, ACS_ULCORNER);
		hline(ACS_HLINE, XLENGTH);
		mvaddch(YBASE - 1,  XBASE + XLENGTH, ACS_URCORNER);

		mvaddch(YBASE + YDEPTH,  XBASE - 1, ACS_LLCORNER);
		hline(ACS_HLINE, XLENGTH);
		mvaddch(YBASE + YDEPTH,  XBASE + XLENGTH, ACS_LRCORNER);

		move(YBASE,  XBASE - 1);
		vline(ACS_VLINE, YDEPTH);

		move(YBASE,  XBASE + XLENGTH);
		vline(ACS_VLINE, YDEPTH);

		attrset(COLOR_PAIR(2));
	}
	do {
		char	buf[30];

		mask = 0;
		time(&now);
		tm = localtime(&now);
		set(tm->tm_sec%10, 0);
		set(tm->tm_sec/10, 4);
		set(tm->tm_min%10, 10);
		set(tm->tm_min/10, 14);
		set(tm->tm_hour%10, 20);
		set(tm->tm_hour/10, 24);
		set(10, 7);
		set(10, 17);
		for(k=0; k<6; k++) {
			if(scrol) {
				for(i=0; i<5; i++)
					new[i] = (new[i]&~mask) | (new[i+1]&mask);
				new[5] = (new[5]&~mask) | (next[k]&mask);
			} else
				new[k] = (new[k]&~mask) | (next[k]&mask);
			next[k] = 0;
			for(s=1; s>=0; s--) {
				standt(s);
				for(i=0; i<6; i++) {
					if((a = (new[i]^old[i])&(s ? new : old)[i]) != 0) {
						for(j=0,t=1<<26; t; t>>=1,j++) {
							if(a&t) {
								if(!(a&(t<<1))) {
									movto(YBASE + i, XBASE + 2*j);
								}
								addstr("  ");
							}
						}
					}
					if(!s) {
						old[i] = new[i];
					}
				}
				if(!s) {
					refresh();
				}
			}
		}

		/* this depends on the detailed format of ctime(3) */
		(void) strcpy(buf, ctime(&now));
		(void) strcpy(buf + 10, buf + 19);
		mvaddstr(16, 30, buf);

		movto(6, 0);
		refresh();
		sleep(1);
		while(wgetch(stdscr) != ERR)
			continue;
		if (sigtermed) {
			standend();
			clear();
			refresh();
			endwin();
			fprintf(stderr, "gdc terminated by signal %d\n", sigtermed);
			return EXIT_FAILURE;
		}
	} while(--n);
	standend();
	clear();
	refresh();
	endwin();
	return EXIT_SUCCESS;
}

static void
set(int t, int n)
{
int i, m;

	m = 7<<n;
	for(i=0; i<5; i++) {
		next[i] |= ((disp[t]>>(4-i)*3)&07)<<n;
		mask |= (next[i]^old[i])&m;
	}
	if(mask&m)
		mask |= m;
}

static void
standt(int on)
{
	if (on) {
		if(hascolor) {
			attron(COLOR_PAIR(1));
		} else {
			attron(A_STANDOUT);
		}
	} else {
		if(hascolor) {
			attron(COLOR_PAIR(2));
		} else {
			attroff(A_STANDOUT);
		}
	}
}

static void
movto(int line, int col)
{
	move(line, col);
}
