
/***************************************************************************
*                            COPYRIGHT NOTICE                              *
****************************************************************************
*                ncurses is copyright (C) 1992-1995                        *
*                          Zeyd M. Ben-Halim                               *
*                          zmbenhal@netcom.com                             *
*                          Eric S. Raymond                                 *
*                          esr@snark.thyrsus.com                           *
*                                                                          *
*        Permission is hereby granted to reproduce and distribute ncurses  *
*        by any means and for any fee, whether alone or as part of a       *
*        larger distribution, in source or in binary form, PROVIDED        *
*        this notice is included with any such distribution, and is not    *
*        removed from any of its header files. Mention of ncurses in any   *
*        applications linked with it is highly appreciated.                *
*                                                                          *
*        ncurses comes AS IS with no warranty, implied or expressed.       *
*                                                                          *
***************************************************************************/


/*
 * Dump control definitions and variables
 */

/* capability output formats */
#define F_TERMINFO	0	/* use terminfo names */
#define F_VARIABLE	1	/* use C variable names */
#define F_TERMCAP	2	/* termcap names with capability conversion */
#define F_TCONVERR	3	/* as T_TERMCAP, no skip of untranslatables */
#define F_LITERAL	4	/* like F_TERMINFO, but no smart defaults */

/* capability sort modes */
#define S_DEFAULT	0	/* sort by terminfo name (implicit) */
#define S_NOSORT	1	/* don't sort */
#define S_TERMINFO	2	/* sort by terminfo names (explicit) */
#define S_VARIABLE	3	/* sort by C variable names */
#define S_TERMCAP	4	/* sort by termcap names */

extern char *nametrans(const char *);
extern void dump_init(const char *, int, int, int, int);
extern int fmt_entry(TERMTYPE *, int (*)(int, int), bool, bool);
extern int dump_entry(TERMTYPE *, bool, int (*)(int, int));
extern int dump_uses(const char *, bool);
extern void compare_entry(void (*)(int, int, const char *));
extern char *expand(char *);

#define FAIL	-1
