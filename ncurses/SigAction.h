
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


/* This file exists to handle non-POSIX systems which don't have <unistd.h>,
 * and usually no sigaction() nor <termios.h>
 */

#ifndef _SIGACTION_H
#define _SIGACTION_H

#if HAVE_LIBC_H
#include <libc.h>
#endif

#undef  SIG_BLOCK
#define SIG_BLOCK       00

#undef  SIG_UNBLOCK
#define SIG_UNBLOCK     01

#undef  SIG_SETMASK
#define SIG_SETMASK     02

 	/*
	 * <bsd/signal.h> is in the Linux 1.2.8 + gcc 2.7.0 configuration,
	 * and is useful for testing this header file.
	 */
#if HAVE_BSD_SIGNAL_H
#include <bsd/signal.h>
#endif

typedef struct sigvec sigaction_t;

#define sigset_t _nc_sigset_t
typedef unsigned long sigset_t;

#undef  sa_mask
#define sa_mask sv_mask
#undef  sa_handler
#define sa_handler sv_handler
#undef  sa_flags
#define sa_flags sv_flags

#undef  sigaction
#define sigaction   _nc_sigaction
#undef  sigprocmask
#define sigprocmask _nc_sigprocmask
#undef  sigemptyset
#define sigemptyset _nc_sigemptyset
#undef  sigsuspend
#define sigsuspend  _nc_sigsuspend
#undef  sigdelset
#define sigdelset   _nc_sigdelset
#undef  sigaddset
#define sigaddset   _nc_sigaddset

extern int sigaction (int sig, sigaction_t * sigact, sigaction_t *  osigact);
extern int sigprocmask (int how, sigset_t *mask, sigset_t *omask);
extern int sigemptyset (sigset_t *mask);
extern int sigsuspend (sigset_t *mask);
extern int sigdelset (sigset_t *mask, int sig);
extern int sigaddset (sigset_t *mask, int sig);

#endif
