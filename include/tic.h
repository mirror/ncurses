
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
 *	tic.h - Global variables and structures for the terminfo
 *			compiler.
 *
 */

#ifndef __TIC_H
#define __TIC_H

#include <curses.h>	/* for the _tracef() prototype, ERR/OK, bool defs */

/*
** The format of compiled terminfo files is as follows:
**
**		Header (12 bytes), containing information given below
**		Names Section, containing the names of the terminal
**		Boolean Section, containing the values of all of the
**				boolean capabilities
**				A null byte may be inserted here to make
**				sure that the Number Section begins on an
**				even word boundary.
**		Number Section, containing the values of all of the numeric
**				capabilities, each as a short integer
**		String Section, containing short integer offsets into the
**				String Table, one per string capability
**		String Table, containing the actual characters of the string
**				capabilities.
**
**	NOTE that all short integers in the file are stored using VAX/PDP-style
**	byte-order, i.e., least-significant byte first.
**
**	There is no structure definition here because it would only confuse
**	matters.  Terminfo format is a raw byte layout, not a structure
**	dump.  If you happen to be on a little-endian machine with 16-bit
**	shorts that requires no padding between short members in a struct,
**	then there is a natural C structure that captures the header, but
**	not very helpfully.
*/

#define MAGIC		0432	/* first two bytes of a compiled entry */
#define MAX_NAME_SIZE	127	/* maximum legal name field size */
#define MAX_ENTRY_SIZE	4096	/* maximum legal entry size */
#define MAX_ALIAS	14	/* maximum size of individual name or alias */

/* location of user's personal info directory */
#define PRIVATE_INFO	"%s/.terminfo"	/* plug getenv("HOME") into %s */

#define DEBUG(n, a)	if (_nc_tracing & (1 << (n - 1))) _tracef a 
extern unsigned _nc_tracing;
extern void _nc_tracef(char *, ...) GCC_PRINTFLIKE(1,2);
extern const char *_nc_visbuf(const char *);

/*
 * These are the types of tokens returned by the scanner.  The first
 * three are also used in the hash table of capability names.  The scanner
 * returns one of these values after loading the specifics into the global
 * structure curr_token.
 */

#define BOOLEAN 0		/* Boolean capability */
#define NUMBER 1		/* Numeric capability */
#define STRING 2		/* String-valued capability */
#define CANCEL 3		/* Capability to be cancelled in following tc's */
#define NAMES  4		/* The names for a terminal type */
#define UNDEF  5		/* Undefined */

#define NO_PUSHBACK	-1	/* used in pushtype to indicate no pushback */

	/*
	 *	The global structure in which the specific parts of a
	 *	scanned token are returned.
	 *
	 */

struct token
{
	char	*tk_name;		/* name of capability */
	int	tk_valnumber;	/* value of capability (if a number) */
	char	*tk_valstring;	/* value of capability (if a string) */
};

extern	struct token	_nc_curr_token;

	/*
	 * The file comp_captab.c contains an array of these structures, one
	 * per possible capability.  These are indexed by a hash table array of
	 * pointers to the same structures for use by the parser.
	 */

struct name_table_entry
{
	const char *nte_name;	/* name to hash on */
	int	nte_type;	/* BOOLEAN, NUMBER or STRING */
	short	nte_index;	/* index of associated variable in its array */
	short	nte_link;	/* index in table of next hash, or -1 */
};

struct alias
{
	const char	*from;
	const char	*to;
	const char	*source;
};

extern const struct name_table_entry * const _nc_info_hash_table[];
extern const struct name_table_entry * const _nc_cap_hash_table[];

extern const struct alias _nc_capalias_table[];
extern const struct alias _nc_infoalias_table[];

extern const struct name_table_entry	*_nc_get_table(bool);

#define NOTFOUND	((struct name_table_entry *) 0)

/* out-of-band values for representing absent capabilities */
#define ABSENT_BOOLEAN		-1
#define ABSENT_NUMERIC		-1
#define ABSENT_STRING		(char *)0

/* out-of-band values for representing cancels */
#define CANCELLED_BOOLEAN	(char)(-2)
#define CANCELLED_NUMERIC	-2
#define CANCELLED_STRING	(char *)-1

/* termcap entries longer than this may break old binaries */
#define MAX_TERMCAP_LENGTH	1023

/* this is a documented limitation of terminfo */
#define MAX_TERMINFO_LENGTH	4096

#ifndef TERMINFO
#define TERMINFO "/usr/share/terminfo"
#endif

/* comp_hash.c: name lookup */
struct name_table_entry	const *_nc_find_entry(const char *,
				    const struct name_table_entry *const *);
struct name_table_entry const *_nc_find_type_entry(const char *,
					 int,
					 const struct name_table_entry *);

/* comp_scan.c: lexical analysis */
extern int  _nc_get_token(void);
extern void _nc_push_token(int);
extern void _nc_reset_input(FILE *, char *);
extern void _nc_panic_mode(char);
extern int _nc_curr_line;
extern int _nc_curr_col;
extern long _nc_curr_file_pos;
extern long _nc_comment_start, _nc_comment_end;
extern int _nc_syntax;
extern long _nc_start_line;
#define SYN_TERMINFO	0
#define SYN_TERMCAP	1

/* comp_error.c: warning & abort messages */
extern void _nc_set_source(const char *const name);
extern void _nc_get_type(char *name);
extern void _nc_set_type(const char *const name);
extern void _nc_syserr_abort(const char *const,...) GCC_PRINTFLIKE(1,2) GCC_NORETURN;
extern void _nc_err_abort(const char *const,...) GCC_PRINTFLIKE(1,2) GCC_NORETURN;
extern void _nc_warning(const char *const,...) GCC_PRINTFLIKE(1,2);
extern bool _nc_suppress_warnings;

/* captoinfo.c: capability conversion */
extern char *_nc_captoinfo(char *const, char *, int const);
extern char *_nc_infotocap(char *const, char *, int const);

/* comp_main.c: compiler main */
extern const char *_nc_progname;

/* read_entry.c */
extern const char *_nc_tic_dir(const char *);

/* write_entry.c */
extern int _nc_tic_written(void);

#endif /* __TIC_H */
