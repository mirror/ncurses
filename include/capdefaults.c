
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

/* $Id: capdefaults.c,v 1.4 1996/08/18 00:56:09 tom Exp $ */

	/*
	 * Compute obsolete capabilities.  The reason this is an include file
	 * is that the two places where it's needed want the macros to
	 * generate offsets to different structures.  See the file Caps for
	 * explanations of these conversions.
	 *
	 * Note: This code is the functional inverse of the first part
	 * of postprocess_entry().
	 */
	{
		char *sp;
		int capval;

#define VALID_STRING(s)	((s) && s != CANCELLED_STRING)
#define EXTRACT_DELAY(str)	(sp = strchr(str, '*'), sp ? atoi(sp+1) : 0)

		/* current (4.4BSD) capabilities marked obsolete */
		if (VALID_STRING(carriage_return)
				&& (capval = EXTRACT_DELAY(carriage_return)))
			carriage_return_delay = capval;
		if (VALID_STRING(newline) && (capval = EXTRACT_DELAY(newline)))
			new_line_delay = capval;

		/* current (4.4BSD) capabilities not obsolete */
		if (!VALID_STRING(termcap_init2) && VALID_STRING(init_3string))
		{
			termcap_init2 = init_3string;
			init_3string = (char *)NULL;
		}
		if (VALID_STRING(reset_1string)
			&& !VALID_STRING(reset_2string)
			&& VALID_STRING(reset_3string))
		{
			termcap_reset = reset_1string;
			reset_1string = (char *)NULL;
		}
		if (magic_cookie_glitch_ul < 0 && magic_cookie_glitch && VALID_STRING(enter_underline_mode))
			magic_cookie_glitch_ul = magic_cookie_glitch;

		/* totally obsolete capabilities */
		linefeed_is_newline = VALID_STRING(newline)
					&& (strcmp("\n", newline) == 0);
		if (VALID_STRING(cursor_left)
				&& (capval = EXTRACT_DELAY(cursor_left)))
			backspace_delay = capval;
		if (VALID_STRING(tab) && (capval = EXTRACT_DELAY(tab)))
			horizontal_tab_delay = capval;
#undef EXTRACT_DELAY
	}
