
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
 * Terminfo-only terminal setup routines:
 *
 *		int restartterm(const char *, int, int *)
 *		TERMINAL *set_curterm(TERMINAL *)
 *		int del_curterm(TERMINAL *)
 */

#include <curses.priv.h>

#ifdef SVR4_TERMIO
#define _POSIX_SOURCE
#endif

#include <term.h>	/* lines, columns, cur_term */

MODULE_ID("$Id: lib_restart.c,v 1.11 1997/02/02 01:10:25 tom Exp $")

#undef tabs

#ifdef TAB3
# define tabs TAB3
#else
# ifdef XTABS
#  define tabs XTABS
# else
#  ifdef OXTABS
#   define tabs OXTABS
#  else
#   define tabs 0
#  endif
# endif
#endif

int def_shell_mode(void)
{
	T((T_CALLED("def_shell_mode()")));

	if (cur_term == 0)
		returnCode(ERR);

	/*
	 * Turn off the XTABS bit in the tty structure if it was on.  If XTABS
	 * was on, remove the tab and backtab capabilities.
	 */

	if (GET_TTY(cur_term->Filedes, &cur_term->Ottyb) == -1)
		returnCode(ERR);
#ifdef TERMIOS
	if (cur_term->Ottyb.c_oflag & tabs)
		tab = back_tab = NULL;
#else
	if (cur_term->Ottyb.sg_flags & XTABS)
		tab = back_tab = NULL;
#endif
	returnCode(OK);
}

int def_prog_mode(void)
{
	T((T_CALLED("def_prog_mode()")));

	if (cur_term == 0)
		returnCode(ERR);

	if (GET_TTY(cur_term->Filedes, &cur_term->Nttyb) == -1)
		returnCode(ERR);
#ifdef TERMIOS
	cur_term->Nttyb.c_oflag &= ~tabs;
#else
	cur_term->Nttyb.sg_flags &= ~XTABS;
#endif
	returnCode(OK);
}

int restartterm(const char *term, int filenum, int *errret)
{
int saveecho = SP->_echo;
int savecbreak = SP->_cbreak;
int saveraw = SP->_raw;
int savenl = SP->_nl;

	T((T_CALLED("restartterm(%s,%d,%p)"), term, filenum, errret));

	setupterm(term, filenum, errret);

	if (saveecho)
		echo();
	else
		noecho();

	if (savecbreak) {
		cbreak();
		noraw();
	} else if (saveraw) {
		nocbreak();
		raw();
	} else {
		nocbreak();
		noraw();
	}
	if (savenl)
		nl();
	else
		nonl();

	reset_prog_mode();

	_nc_get_screensize();

	returnCode(OK);
}

TERMINAL *set_curterm(TERMINAL *term)
{
	TERMINAL	*oldterm = cur_term;

	cur_term = term;
	return oldterm;
}

int del_curterm(TERMINAL *term)
{
	T((T_CALLED("del_curterm(%p)"), term));

	if (term != NULL) {
		FreeIfNeeded(term->type.str_table);
		FreeIfNeeded(term->type.term_names);
		free(term);
		returnCode(OK);
	}
	returnCode(ERR);
}
