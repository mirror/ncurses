/****************************************************************************
 * Copyright (c) 1998 Free Software Foundation, Inc.                        *
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
 ****************************************************************************/

/*
**	lib_clrbot.c
**
**	The routine wclrtobot().
**
*/

#include <curses.priv.h>

MODULE_ID("$Id: lib_clrbot.c,v 1.13 1998/02/11 12:13:54 tom Exp $")

int wclrtobot(WINDOW *win)
{
int     code = ERR;
chtype	blank;
chtype	*ptr, *end;
short	y, startx;

	T((T_CALLED("wclrtobot(%p)"), win));

	if (win) {
	  startx = win->_curx;
	  
	  T(("clearing from y = %d to y = %d with maxx =  %d", win->_cury, win->_maxy, win->_maxx));
	  
	  for (y = win->_cury; y <= win->_maxy; y++) {
	    end = &win->_line[y].text[win->_maxx];
	    
	    blank = _nc_background(win);
	    for (ptr = &win->_line[y].text[startx]; ptr <= end; ptr++)
	      *ptr = blank;
	    
	    if (win->_line[y].firstchar > startx
		||  win->_line[y].firstchar == _NOCHANGE)
	      win->_line[y].firstchar = startx;
	    
	    win->_line[y].lastchar = win->_maxx;
	    
	    startx = 0;
	  }
	  _nc_synchook(win);
	  code = OK;
	}
	returnCode(code);
}

