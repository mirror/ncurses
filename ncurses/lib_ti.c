
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


#include <curses.priv.h>

#include <term.h>
#include <tic.h>

MODULE_ID("$Id: lib_ti.c,v 1.7 1996/12/21 14:24:06 tom Exp $")

int tigetflag(const char *str)
{
int i;

	T(("tigetflag(%s)", str));

	for (i = 0; i < BOOLCOUNT; i++)
		if (!strcmp(str, boolnames[i]))
			return cur_term->type.Booleans[i];

	return ABSENT_BOOLEAN;
}

int tigetnum(const char *str)
{
int i;

	T(("tigetnum(%s)", str));

	for (i = 0; i < NUMCOUNT; i++)
		if (!strcmp(str, numnames[i]))
			return cur_term->type.Numbers[i];

	return CANCELLED_NUMERIC;
}

char *tigetstr(const char *str)
{
int i;

	T(("tigetstr(%s)", str));

	for (i = 0; i < STRCOUNT; i++)
		if (!strcmp(str, strnames[i]))
			return cur_term->type.Strings[i];

	return CANCELLED_STRING;
}
