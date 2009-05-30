/****************************************************************************
 * Copyright (c) 1998-2008,2009 Free Software Foundation, Inc.              *
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

/****************************************************************************
 *  Author: Thomas E. Dickey <dickey@clark.net> 1997                        *
 ****************************************************************************/
/*
 * Module that "owns" the 'cur_term' variable:
 *
 *	TERMINAL *set_curterm(TERMINAL *)
 *	int del_curterm(TERMINAL *)
 */

#include <curses.priv.h>
#include <term_entry.h>		/* TTY, cur_term */
#include <termcap.h>		/* ospeed */

MODULE_ID("$Id: lib_cur_term.c,v 1.22 2009/05/30 13:55:19 tom Exp $")

#undef CUR
#define CUR termp->type.

#if BROKEN_LINKER && !USE_REENTRANT
NCURSES_EXPORT_VAR(TERMINAL *) cur_term = 0;
#elif BROKEN_LINKER || USE_REENTRANT
NCURSES_EXPORT(TERMINAL *)
NCURSES_PUBLIC_VAR(cur_term) (void)
{
    return (SP != 0 && SP->_term != 0) ? SP->_term : _nc_prescreen._cur_term;
}
#else
NCURSES_EXPORT_VAR(TERMINAL *) cur_term = 0;
#endif

NCURSES_EXPORT(TERMINAL *)
set_curterm(TERMINAL * termp)
{
    TERMINAL *oldterm;

    T((T_CALLED("set_curterm(%p)"), termp));

    _nc_lock_global(curses);
    oldterm = cur_term;
    if (SP)
	SP->_term = termp;
#if BROKEN_LINKER && !USE_REENTRANT
    cur_term = termp;
#elif BROKEN_LINKER || USE_REENTRANT
    _nc_prescreen._cur_term = termp;
#else
    cur_term = termp;
#endif
    if (termp != 0) {
	ospeed = _nc_ospeed(termp->_baudrate);
	if (termp->type.Strings) {
	    PC = (char) ((pad_char != NULL) ? pad_char[0] : 0);
	}
    }
    _nc_unlock_global(curses);

    T((T_RETURN("%p"), oldterm));
    return (oldterm);
}

NCURSES_EXPORT(int)
NCURSES_SP_NAME(del_curterm) (NCURSES_SP_DCLx TERMINAL * termp)
{
    int rc = ERR;

    T((T_CALLED("del_curterm(%p)"), termp));

    _nc_lock_global(curses);
    if (termp != 0) {
	_nc_free_termtype(&(termp->type));
	FreeIfNeeded(termp->_termname);
#if USE_HOME_TERMINFO
	if (_nc_globals.home_terminfo != 0)
	    FreeAndNull(_nc_globals.home_terminfo);
#endif
	free(termp);
	if (termp == cur_term)
	    set_curterm(0);
	rc = OK;
    }
    _nc_unlock_global(curses);

    returnCode(rc);
}

#if NCURSES_SP_FUNCS
NCURSES_EXPORT(int)
del_curterm(TERMINAL * termp)
{
    int rc = ERR;
    rc = NCURSES_SP_NAME(del_curterm) (CURRENT_SCREEN, termp);
    return (rc);
}
#endif
