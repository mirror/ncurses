
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
 *	term_entry.h -- interface to entry-manipulation code
 */

#ifndef _TERM_ENTRY_H
#define _TERM_ENTRY_H

#define MAX_USES	32

typedef struct entry {
	TERMTYPE	tterm;
	int		nuses;
	struct
        {
	    void	*parent;	/* (char *) or (ENTRY *) */
	    long	line;
        }
	uses[MAX_USES];
	long		cstart, cend;
	long		startline;
	struct entry	*next;
	struct entry	*last;
}
ENTRY;

extern ENTRY	*_nc_head, *_nc_tail;
#define for_entry_list(qp)	for (qp = _nc_head; qp; qp = qp->next)

#define MAX_LINE	132

#define NULLHOOK        (bool(*)(ENTRY *))0

/* alloc_entry.c: elementary allocation code */
extern void _nc_init_entry(TERMTYPE *const);
extern char *_nc_save_str(const char *const);
extern void _nc_merge_entry(TERMTYPE *const, TERMTYPE *const);
extern void _nc_wrap_entry(ENTRY *const);

/* parse_entry.c: entry-parsing code */
extern int _nc_parse_entry(ENTRY *, int, bool);
extern int _nc_capcmp(const char *, const char *);

/* write_entry.c: writing an entry to the file system */
extern void _nc_set_writedir(char *);
extern void _nc_write_entry(TERMTYPE *const);

/* comp_parse.c: entry list handling */
extern void _nc_read_entry_source(FILE*, char*, int, bool, bool (*)(ENTRY*));
extern bool _nc_entry_match(char *, char *);
extern int _nc_resolve_uses(void);
extern void _nc_free_entries(ENTRY *);

#endif /* _TERM_ENTRY_H */

/* term_entry.h ends here */
