
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
**	lib_insch.c
**
**	The routine winsch().
**
*/

#include <curses.priv.h>

MODULE_ID("$Id: lib_insch.c,v 1.7 1997/04/12 17:43:02 tom Exp $")

int  winsch(WINDOW *win, chtype c)
{
chtype	*temp1, *temp2;
chtype	*end;

	T((T_CALLED("winsch(%p, %s)"), win, _tracechtype(c)));

	end = &win->_line[win->_cury].text[win->_curx];
	temp1 = &win->_line[win->_cury].text[win->_maxx];
	temp2 = temp1 - 1;

	while (temp1 > end)
	    *temp1-- = *temp2--;

	*temp1 = _nc_render(win, c);

	win->_line[win->_cury].lastchar = win->_maxx;
	if (win->_line[win->_cury].firstchar == _NOCHANGE
	    			||  win->_line[win->_cury].firstchar > win->_curx)
	    win->_line[win->_cury].firstchar = win->_curx;
	returnCode(OK);
}
