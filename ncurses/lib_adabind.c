
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
**	lib_adabind.c
**
**	Some small wrappers to ease the implementation of an Ada95
**      binding. Especially functionalities only available as macros
**      in (n)curses are wrapped here by functions. 
**      See the documentation and copyright notices in the ../Ada95
**      subdirectory.
*/
#include "curses.priv.h"

/*  In (n)curses are a few functionalities that can't be expressed as 
//  functions, because for historic reasons they use as macro argument
//  variable names that are "out" parameters. For those macros we provide
//  small wrappers.
*/

/* Prototypes for the functions in this module */
int  _nc_ada_getmaxyx (WINDOW *win, int *y, int *x);
int  _nc_ada_getbegyx (WINDOW *win, int *y, int *x);
int  _nc_ada_getyx (WINDOW *win, int *y, int *x);
int  _nc_ada_getparyx (WINDOW *win, int *y, int *x);
int  _nc_ada_isscroll (WINDOW *win);
int  _nc_ada_coord_transform (WINDOW *win, int *Y, int *X, int dir);
void _nc_ada_mouse_event (mmask_t m, int *b, int *s);
int  _nc_ada_mouse_mask (int button, int state, mmask_t *mask);
void _nc_ada_unregister_mouse (void);


int _nc_ada_getmaxyx (WINDOW *win, int *y, int *x)
{
  if (win && y && x)
    {
      getmaxyx(win,*y,*x);
      return OK;
    }
  else
    return ERR;
}

int _nc_ada_getbegyx (WINDOW *win, int *y, int *x)
{
  if (win && y && x)
    {
      getbegyx(win,*y,*x);
      return OK;
    }
  else
    return ERR;
}

int _nc_ada_getyx (WINDOW *win, int *y, int *x)
{
  if (win && y && x)
    {
      getyx(win,*y,*x);
      return OK;
    }
  else
    return ERR;
}

int _nc_ada_getparyx (WINDOW *win, int *y, int *x)
{
  if (win && y && x)
    {
      getparyx(win,*y,*x);
      return OK;
    }
  else
    return ERR;
}

int _nc_ada_isscroll (WINDOW *win)
{
  return win ? (win->_scroll ? TRUE : FALSE) : ERR;
}

int _nc_ada_coord_transform( WINDOW *win, int *Y, int *X, int dir)
{
  if (win && Y && X)
    {
      int y = *Y; int x = *X;
      if (dir)
	{ /* to screen coordinates */
	  y += win->_yoffset;
	  y += win->_begy;
	  x += win->_begx;
	  if (!wenclose(win,y,x))
	    return FALSE;
	}
      else
	{ /* from screen coordinates */
	  if (!wenclose(win,y,x))
	    return FALSE;
	  y -= win->_yoffset;
	  y -= win->_begy;
	  x -= win->_begx;
	}
      *X = x;
      *Y = y;
      return TRUE;
    }
  return FALSE;
}

#define BUTTON1_EVENTS (BUTTON1_RELEASED        |\
                        BUTTON1_PRESSED         |\
                        BUTTON1_CLICKED         |\
                        BUTTON1_DOUBLE_CLICKED  |\
                        BUTTON1_TRIPLE_CLICKED  |\
                        BUTTON1_RESERVED_EVENT  )

#define BUTTON2_EVENTS (BUTTON2_RELEASED        |\
                        BUTTON2_PRESSED         |\
                        BUTTON2_CLICKED         |\
                        BUTTON2_DOUBLE_CLICKED  |\
                        BUTTON2_TRIPLE_CLICKED  |\
                        BUTTON2_RESERVED_EVENT  )

#define BUTTON3_EVENTS (BUTTON3_RELEASED        |\
                        BUTTON3_PRESSED         |\
                        BUTTON3_CLICKED         |\
                        BUTTON3_DOUBLE_CLICKED  |\
                        BUTTON3_TRIPLE_CLICKED  |\
                        BUTTON3_RESERVED_EVENT  )

#define BUTTON4_EVENTS (BUTTON4_RELEASED        |\
                        BUTTON4_PRESSED         |\
                        BUTTON4_CLICKED         |\
                        BUTTON4_DOUBLE_CLICKED  |\
                        BUTTON4_TRIPLE_CLICKED  |\
                        BUTTON4_RESERVED_EVENT  )

void _nc_ada_mouse_event( mmask_t m, int *b, int *s )
{
  int k = 0;

  if ( m & BUTTON1_EVENTS)
    {
      k = 1;
    }
  else if ( m & BUTTON2_EVENTS)
    {
      k = 2;
    }
  else if ( m & BUTTON3_EVENTS)
    {
      k = 3;
    }
  else if ( m & BUTTON4_EVENTS)
    {
      k = 4;
    }

  if (k)
    {
      *b = k-1;
      if (BUTTON_RELEASE(m,k)) *s = 0;
      else if (BUTTON_PRESS(m,k)) *s = 1;
      else if (BUTTON_CLICK(m,k)) *s = 2;
      else if (BUTTON_DOUBLE_CLICK(m,k)) *s = 3;
      else if (BUTTON_TRIPLE_CLICK(m,k)) *s = 4;
      else if (BUTTON_RESERVED_EVENT(m,k)) *s = 5;
      else
	{
	  *s = -1;
	}
    }
  else
    {
      *s = 1;
      if (m & BUTTON_CTRL) *b = 4;
      else if (m & BUTTON_SHIFT) *b = 5;
      else if (m & BUTTON_ALT) *b = 6;
      else
	{
	  *b = -1;
	}
    }
}

int _nc_ada_mouse_mask ( int button, int state, mmask_t *mask )
{
  mmask_t b = (button<4) ? ((1<<button) << (6 * state)) :
    (BUTTON_CTRL << (button-4));

  if (button>=4 && state!=1)
    return ERR;

  *mask |= b;
  return OK;
}

void _nc_ada_unregister_mouse (void)
{
  _nc_mouse_wrap(SP);
}

