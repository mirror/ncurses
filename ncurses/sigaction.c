
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

#include <curses.priv.h>

/* This file provides sigaction() emulation using sigvec() */
/* Use only if this is non POSIX system */

#if !HAVE_SIGACTION
#include <signal.h>
#include <SigAction.h>

MODULE_ID("$Id: sigaction.c,v 1.6 1996/07/31 00:15:36 tom Exp $")

int
sigaction (int sig, sigaction_t * sigact, sigaction_t * osigact)
{
  return sigvec(sig, sigact, osigact);
}

int
sigemptyset (sigset_t * mask)
{
  *mask = 0;
  return 0;
}

int
sigprocmask (int mode, sigset_t * mask, sigset_t * omask)
   {
   sigset_t current = sigsetmask(0);

   if (omask) *omask = current;

   if (mode==SIG_BLOCK)
      current |= *mask;
   else if (mode==SIG_UNBLOCK)
      current &= ~*mask;
   else if (mode==SIG_SETMASK)
      current = *mask;

   sigsetmask(current);
   return 0;
   }

int
sigsuspend (sigset_t * mask)
{
  return sigpause (*mask);
}

int
sigdelset (sigset_t * mask, int sig)
{
  *mask &= ~sigmask (sig);
  return 0;
}

int
sigaddset (sigset_t * mask, int sig)
{
  *mask |= sigmask (sig);
  return 0;
}
#else
extern void _nc_sigaction(void);	/* quiet's gcc warning */
void _nc_sigaction(void) { } /* nonempty for strict ANSI compilers */
#endif
