/******************************************************************************
 * Copyright 1996 by Thomas E. Dickey <dickey@clark.net>                      *
 * All Rights Reserved.                                                       *
 *                                                                            *
 * Permission to use, copy, modify, and distribute this software and its      *
 * documentation for any purpose and without fee is hereby granted, provided  *
 * that the above copyright notice appear in all copies and that both that    *
 * copyright notice and this permission notice appear in supporting           *
 * documentation, and that the name of the above listed copyright holder(s)   *
 * not be used in advertising or publicity pertaining to distribution of the  *
 * software without specific, written prior permission. THE ABOVE LISTED      *
 * COPYRIGHT HOLDER(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  *
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO     *
 * EVENT SHALL THE ABOVE LISTED COPYRIGHT HOLDER(S) BE LIABLE FOR ANY         *
 * SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER       *
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF       *
 * CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN        *
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.                   *
 ******************************************************************************/

#define HAVE_NC_FREEALL 1

#include <curses.priv.h>
#include <term.h>

#if HAVE_LIBDBMALLOC
extern int malloc_errfd;	/* FIXME */
#endif

MODULE_ID("$Id: lib_freeall.c,v 1.8 1997/02/15 18:53:43 tom Exp $")

static void free_slk(SLK *p)
{
	if (p != 0) {
		FreeIfNeeded(p->ent);
		FreeIfNeeded(p->buffer);
		free(p);
	}
}

void _nc_free_termtype(struct termtype *p, int base)
{
	if (p != 0) {
		FreeIfNeeded(p->term_names);
		FreeIfNeeded(p->str_table);
		if (base)
			free(p);
	}
}

static void free_tries(struct tries *p)
{
	struct tries *q;

	while (p != 0) {
		q = p->sibling;
		if (p->child != 0)
			free_tries(p->child);
		free(p);
		p = q;
	}
}

/*
 * Free all ncurses data.  This is used for testing only (there's no practical
 * use for it as an extension).
 */
void _nc_freeall(void)
{
	WINDOWLIST *p, *q;

#if NO_LEAKS
	_nc_free_tparm();
#endif
	while (_nc_windows != 0) {
		/* Delete only windows that're not a parent */
		for (p = _nc_windows; p != 0; p = p->next) {
			bool found = FALSE;

			for (q = _nc_windows; q != 0; q = q->next) {
				if ((p != q)
				 && (q->win->_flags & _SUBWIN)
				 && (p->win == q->win->_parent)) {
					found = TRUE;
					break;
				}
			}

			if (!found) {
				delwin(p->win);
				break;
			}
		}
	}

	if (SP != 0) {
		free_tries (SP->_keytry);
	    	free_slk(SP->_slk);
		FreeIfNeeded(SP->_color_pairs);
		FreeIfNeeded(SP->_color_table);
		_nc_set_buffer(SP->_ofp, FALSE);
#if !BROKEN_LINKER
		FreeAndNull(SP);
#endif
	}

	if (cur_term != 0) {
		_nc_free_termtype(&(cur_term->type), TRUE);
	}

#if HAVE_LIBDBMALLOC
	malloc_dump(malloc_errfd);
#elif HAVE_LIBDMALLOC
#elif HAVE_PURIFY
	purify_all_inuse();
#endif
}

void _nc_free_and_exit(int code)
{
	_nc_freeall();
	exit(code);
}
