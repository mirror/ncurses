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
 *  Author: Thomas E. Dickey <dickey@clark.net> 1997                        *
 ****************************************************************************/

/*
 *	lib_redrawln.c
 *
 *	The routine wredrawln().
 *
 */

#include <curses.priv.h>

MODULE_ID("$Id: lib_redrawln.c,v 1.2 1998/02/11 12:13:57 tom Exp $")

int wredrawln(WINDOW *win, int beg, int num)
{
int i;

	T((T_CALLED("wredrawln(%p,%d,%d)"), win, beg, num));

	if (touchline(win, beg, num) == OK) {
		size_t len = win->_maxx * sizeof(chtype);

		/*
		 * XSI says that wredrawln() tells the library not to base
		 * optimization on the contents of the lines that are marked.
		 * We do that by changing the contents to nulls after touching
		 * the corresponding lines to get the optimizer's attention.
		 *
		 * FIXME: this won't work if the application makes further
		 * updates before the next refresh.
		 */
		for (i = beg; (i < beg + num) && (i < win->_maxy); i++) {
			memset(win->_line[i].text, 0, len);
		}
	}
	returnCode(OK);
}
