/****************************************************************************
 * Copyright (c) 1998-2003,2009 Free Software Foundation, Inc.              *
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
 *  Author: Zeyd M. Ben-Halim <zmbenhal@netcom.com> 1992,1995               *
 *     and: Eric S. Raymond <esr@snark.thyrsus.com>                         *
 *     and: Thomas E. Dickey                        1996-2003               *
 *     and: Juergen Pfeifer                         2009                    *
 ****************************************************************************/

/*
**	lib_has_cap.c
**
**	The routines to query terminal capabilities
**
*/

#include <curses.priv.h>

#include <term.h>

MODULE_ID("$Id: lib_has_cap.c,v 1.5 2009/02/15 00:47:12 tom Exp $")

NCURSES_EXPORT(bool)
NCURSES_SP_NAME(has_ic) (NCURSES_SP_DCL0)
{
    T((T_CALLED("has_ic()")));
    returnCode(cur_term &&
	       (insert_character || parm_ich
		|| (enter_insert_mode && exit_insert_mode))
	       && (delete_character || parm_dch));
}

#if NCURSES_SP_FUNCS
NCURSES_EXPORT(bool)
has_ic(void)
{
    return NCURSES_SP_NAME(has_ic) (CURRENT_SCREEN);
}
#endif

NCURSES_EXPORT(bool)
NCURSES_SP_NAME(has_il) (NCURSES_SP_DCL0)
{
    T((T_CALLED("has_il()")));
    returnCode(cur_term
	       && (insert_line || parm_insert_line)
	       && (delete_line || parm_delete_line));
}

#if NCURSES_SP_FUNCS
NCURSES_EXPORT(bool)
has_il(void)
{
    return NCURSES_SP_NAME(has_il) (CURRENT_SCREEN);
}
#endif
