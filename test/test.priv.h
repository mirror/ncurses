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
/* $Id: test.priv.h,v 1.13 1997/04/06 01:44:04 tom Exp $ */
#if HAVE_CONFIG_H
#include <ncurses_cfg.h>
#endif

#include <stdlib.h>
#include <sys/types.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <curses.h>

#ifdef NCURSES_NOMACROS
#include <nomacros.h>
#endif

#if HAVE_GETOPT_H
#include <getopt.h>
#else
/* 'getopt()' may be prototyped in <stdlib.h>, but declaring its variables
 * doesn't hurt.
 */
extern char *optarg;
extern int optind;
#endif /* HAVE_GETOPT_H */

#ifndef GCC_NORETURN
#define GCC_NORETURN /* nothing */
#endif
#ifndef GCC_UNUSED
#define GCC_UNUSED /* nothing */
#endif

#if defined(NCURSES_VERSION) && HAVE_NC_ALLOC_H
#include <nc_alloc.h>
#endif

#ifndef ExitProgram
#define ExitProgram(code) return code
#endif

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

/* Use this to quiet gcc's -Wwrite-strings warnings, but accommodate SVr4
 * curses which doesn't have const parameters declared (so far) in the places
 * that XSI shows.
 */
#ifndef NCURSES_CONST
#define NCURSES_CONST /* nothing */
#endif
