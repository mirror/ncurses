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


#include <curses.priv.h>

#include <term.h>
#include <tic.h>

MODULE_ID("$Id: lib_ti.c,v 1.9 1998/02/11 12:14:00 tom Exp $")

int tigetflag(const char *str)
{
int i;

	T(("tigetflag(%s)", str));

	if (cur_term != 0)
		for (i = 0; i < BOOLCOUNT; i++)
			if (!strcmp(str, boolnames[i]))
				return cur_term->type.Booleans[i];

	return ABSENT_BOOLEAN;
}

int tigetnum(const char *str)
{
int i;

	T(("tigetnum(%s)", str));

	if (cur_term != 0)
		for (i = 0; i < NUMCOUNT; i++)
			if (!strcmp(str, numnames[i]))
				return cur_term->type.Numbers[i];

	return CANCELLED_NUMERIC;
}

char *tigetstr(const char *str)
{
int i;

	T(("tigetstr(%s)", str));

	if (cur_term != 0)
		for (i = 0; i < STRCOUNT; i++)
			if (!strcmp(str, strnames[i]))
				return cur_term->type.Strings[i];

	return CANCELLED_STRING;
}
