
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
 * $Id: progs.priv.h,v 1.9 1997/04/05 23:38:08 tom Exp $
 *
 *	progs.priv.h
 *
 *	Header file for curses utility programs
 *
 */

#include <ncurses_cfg.h>

#ifdef USE_RCS_IDS
#define MODULE_ID(id) static const char Ident[] = id;
#else
#define MODULE_ID(id) /*nothing*/
#endif

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#else
# if HAVE_LIBC_H
# include <libc.h>
# endif
#endif

#if HAVE_SYS_BSDTYPES_H
#include <sys/bsdtypes.h>	/* needed for ISC */
#endif

#if HAVE_LIMITS_H
# include <limits.h>
#elif HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif

#ifndef PATH_MAX
# if defined(_POSIX_PATH_MAX)
#  define PATH_MAX _POSIX_PATH_MAX
# elif defined(MAXPATHLEN)
#  define PATH_MAX MAXPATHLEN
# else
#  define PATH_MAX 255	/* the Posix minimum pathsize */
# endif
#endif

#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#include <errno.h>

#if !HAVE_EXTERN_ERRNO
extern int errno;
#endif

#if HAVE_GETOPT_H
#include <getopt.h>
#else
/* 'getopt()' may be prototyped in <stdlib.h>, but declaring its
 * variables doesn't hurt.
 */
extern char *optarg;
extern int optind;
#endif /* HAVE_GETOPT_H */

#include <term.h>
#include <tic.h>
#include <nc_alloc.h>

/* usually in <unistd.h> */
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif

#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

#ifndef F_OK
#define	F_OK	0		/* Test for existence.  */
#endif

/* usually in <unistd.h> */
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif

#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif
