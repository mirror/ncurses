
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
 * clear.c --  clears the terminal's screen
 */

#include <progs.priv.h>

#include <curses.h>

MODULE_ID("$Id: clear.c,v 1.6 1996/12/21 17:35:11 tom Exp $")

static int putch(int c)
{
	return putchar(c);
}

int main(
	int argc GCC_UNUSED,
	char *argv[] GCC_UNUSED)
{
	setupterm((char *) 0, STDOUT_FILENO, (int *) 0);
	if (clear_screen == (char *) 0)
		return EXIT_FAILURE;
	tputs(clear_screen, lines > 0 ? lines : 1, putch);
	return EXIT_SUCCESS;
}
