
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
 *	tputs.c
 *		delay_output()
 *		_nc_outch()
 *		tputs()
 *
 */

#include <curses.priv.h>
#include <ctype.h>
#include <term.h>	/* padding_baud_rate, xon_xoff */
#include <tic.h>

MODULE_ID("$Id: lib_tputs.c,v 1.18 1997/02/02 01:52:39 tom Exp $")

int delay_output(int ms)
{
	T((T_CALLED("delay_output(%d)"), ms));

	if (SP == 0 || SP->_baudrate <= 0)
		returnCode(ERR);
#ifdef no_pad_char
	else if (no_pad_char)
		napms(ms);
#endif /* no_pad_char */
	else {
		register int	nullcount;
		char	null = '\0';

#ifdef pad_char
		if (pad_char)
			null = pad_char[0];
#endif /* pad_char */

		for (nullcount = ms * 1000 / SP->_baudrate; nullcount > 0; nullcount--)
			putc(null, SP->_ofp);
		(void) fflush(SP->_ofp);
	}

	returnCode(OK);
}

int _nc_outch(int ch)
{
#ifdef TRACE
    	_nc_outchars++;
#endif /* TRACE */

	if (SP != NULL)
		putc(ch, SP->_ofp);
	else
		putc(ch, stdout);
	return OK;
}

int putp(const char *string)
{
	return tputs(string, 1, _nc_outch);
}

int tputs(const char *string, int affcnt, int (*outc)(int))
{
float	number;
#ifdef BSD_TPUTS
float	trailpad;
#endif /* BSD_TPUTS */

#ifdef TRACE
char	addrbuf[17];

	if (_nc_tracing & TRACE_TPUTS)
	{
		if (outc == _nc_outch)
			(void) strcpy(addrbuf, "_nc_outch");
		else
			(void) sprintf(addrbuf, "%p", outc);
		if (_nc_tputs_trace)
			TR(TRACE_MAXIMUM, ("tputs(%s = %s, %d, %s) called", _nc_tputs_trace, _nc_visbuf(string), affcnt, addrbuf));
		else
			TR(TRACE_MAXIMUM, ("tputs(%s, %d, %s) called", _nc_visbuf(string), affcnt, addrbuf));
		_nc_tputs_trace = (char *)NULL;
	}
#endif /* TRACE */

	if (string == ABSENT_STRING || string == CANCELLED_STRING)
		return ERR;

#ifdef BSD_TPUTS
	/*
	 * This ugly kluge deals with the fact that some ancient BSD programs
	 * (like nethack) actually do the likes of tputs("50") to get delays.
	 */
	trailpad = 0;
	number = 0;

	while (isdigit(*string)) {
		trailpad = trailpad * 10 + *string - '0';
		string++;
	}

	if (*string == '.') {
		string++;
		if (isdigit(*string)) {
			trailpad += (float) (*string - '0') / 10.;
			string++;
		}
		while (isdigit(*string))
			string++;
	}

	if (*string == '*') {
		trailpad *= affcnt;
		string++;
	}
#endif /* BSD_TPUTS */

	while (*string) {
		if (*string != '$')
			(*outc)(*string);
		else {
			string++;
			if (*string != '<') {
				(*outc)('$');
				if (*string)
				    (*outc)(*string);
			} else {
				bool mandatory;

				number = 0;
				string++;

				if ((!isdigit(*string) && *string != '.') || !strchr(string, '>')) {
					(*outc)('$');
					(*outc)('<');
					continue;
				}
				while (isdigit(*string)) {
					number = number * 10 + *string - '0';
					string++;
				}

				if (*string == '.') {
					string++;
					if (isdigit(*string)) {
						number += (float) (*string - '0') / 10.;
						string++;
					}
					while (isdigit(*string))
						string++;
				}

				mandatory = !xon_xoff;
				while (*string == '*' || *string == '/')
				{
					if (*string == '*') {
						number *= affcnt;
						string++;
					}
					else /* if (*string == '/') */ {
						mandatory = TRUE;
						string++;
					}
				}

#ifdef padding_baud_rate
				if (mandatory && number > 0 && padding_baud_rate && (!SP || SP->_baudrate >= padding_baud_rate))
					delay_output(number);
#endif /* padding_baud_rate */
				number = 0;

			} /* endelse (*string == '<') */
		} /* endelse (*string == '$') */

		if (*string == '\0')
			break;

		string++;
	}

#ifdef BSD_TPUTS
	/*
	 * Emit any BSD-style prefix padding that we've accumulated now.
	 */
#ifdef padding_baud_rate
	if (trailpad > 0 && !xon_xoff && padding_baud_rate && (!SP || SP->_baudrate >= padding_baud_rate))
		delay_output(number);
#endif /* padding_baud_rate */
#endif /* BSD_TPUTS */

	return OK;
}
