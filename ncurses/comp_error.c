
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
 *	comp_error.c -- Error message routines
 *
 */

#include <curses.priv.h>

#include <tic.h>

MODULE_ID("$Id: comp_error.c,v 1.12 1996/12/21 14:24:06 tom Exp $")

bool _nc_suppress_warnings;

static const char *sourcename;
static char termtype[MAX_NAME_SIZE+1];

void _nc_set_source(const char *const name)
{
	sourcename = name;
}

void _nc_set_type(const char *const name)
{
	if (name)
		strncpy( termtype, name, MAX_NAME_SIZE );
	else
		termtype[0] = '\0';
}

void _nc_get_type(char *name)
{
	strcpy( name, termtype );
}

static inline void where_is_problem(void)
{
	fprintf (stderr, "\"%s\"", sourcename);
	if (_nc_curr_line >= 0)
	    fprintf (stderr, ", line %d", _nc_curr_line);
	if (_nc_curr_col >= 0)
		fprintf (stderr, ", col %d", _nc_curr_col);
	if (termtype[0])
		fprintf (stderr, ", terminal '%s'", termtype);
	fputc(':', stderr);
	fputc(' ', stderr);
}

void _nc_warning(const char *const fmt, ...)
{
va_list argp;

	if (_nc_suppress_warnings)
	    return;

	where_is_problem();
	va_start(argp,fmt);
	vfprintf (stderr, fmt, argp);
	fprintf (stderr, "\n");
	va_end(argp);
}


void _nc_err_abort(const char *const fmt, ...)
{
va_list argp;

	where_is_problem();
	va_start(argp,fmt);
	vfprintf (stderr, fmt, argp);
	fprintf (stderr, "\n");
	va_end(argp);
	exit(EXIT_FAILURE);
}


void _nc_syserr_abort(const char *const fmt, ...)
{
va_list argp;

	where_is_problem();
	va_start(argp,fmt);
	vfprintf (stderr, fmt, argp);
	fprintf (stderr, "\n");
	va_end(argp);
	abort();
}
