// * This makes emacs happy -*-Mode: C++;-*-
#ifndef _CURSESW_H
#define _CURSESW_H

#include <ncurses_cfg.h>

#pragma interface

#include <stdio.h>
#include <stdarg.h>

#if defined(__GNUG__)
#  if HAVE_BUILTIN_H
#    define exception builtin_exception
#    include <builtin.h>
#    undef exception
#  endif
#else  // #elif defined (__SUNPRO_CC)
#  include <generic.h>
#  include <string.h>
   extern "C" { unsigned sleep(int); }
#endif

#if HAVE_VALUES_H
#  include <values.h>
#endif

#include <strstream.h>
#include <etip.h>

extern "C" {
#  include   <curses.h>
}

/* SCO 3.2v4 curses.h includes term.h, which defines lines as a macro.
   Undefine it here, because NCursesWindow uses lines as a method.  */
#undef lines

/* "Convert" macros to inlines. We'll define it as another symbol to avoid
 * conflict with library symbols.
 */
#undef UNDEF
#define UNDEF(name) CUR_ ##name

#ifdef addch
inline int UNDEF(addch)(chtype ch)  { return addch(ch); }
#undef addch
#define addch UNDEF(addch)
#endif

#ifdef addstr
/* The (char*) cast is to hack around missing const's */
inline int UNDEF(addstr)(const char * str)  { return addstr((char*)str); }
#undef addstr
#define addstr UNDEF(addstr)
#endif

#ifdef attron
inline int UNDEF(attron)(chtype at) { return attron(at); }
#undef attron
#define attron UNDEF(attron)
#endif

#ifdef attroff
inline int UNDEF(attroff)(chtype at) { return attroff(at); }
#undef attroff
#define attroff UNDEF(attroff)
#endif

#ifdef attrset
inline chtype UNDEF(attrset)(chtype at) { return attrset(at); }
#undef attrset
#define attrset UNDEF(attrset)
#endif

#ifdef border
inline int UNDEF(border)(chtype ls, chtype rs, chtype ts, chtype bs, chtype tl, chtype tr, chtype bl, chtype br)
{ return border(ls,rs,ts,bs,tl,tr,bl,br); }
#undef border
#define border UNDEF(border)
#endif

#ifdef box
inline int UNDEF(box)(WINDOW *win, int v, int h) { return box(win, v, h); }
#undef box
#define box UNDEF(box)
#endif

#ifdef clear
inline int UNDEF(clear)()  { return clear(); }
#undef clear
#define clear UNDEF(clear)
#endif

#ifdef clearok
inline int UNDEF(clearok)(WINDOW* win, int bf)  { return clearok(win, bf); }
#undef clearok
#define clearok UNDEF(clearok)
#else
extern "C" int clearok(WINDOW*, bool);
#endif

#ifdef clrtobot
inline int UNDEF(clrtobot)()  { return clrtobot(); }
#undef clrtobot
#define clrtobot UNDEF(clrtobot)
#endif

#ifdef clrtoeol
inline int UNDEF(clrtoeol)()  { return clrtoeol(); }
#undef clrtoeol
#define clrtoeol UNDEF(clrtoeol)
#endif

#ifdef delch
inline int UNDEF(delch)()  { return delch(); }
#undef delch
#define delch UNDEF(delch)
#endif

#ifdef deleteln
inline int UNDEF(deleteln)()  { return deleteln(); }
#undef deleteln
#define deleteln UNDEF(deleteln)
#endif

#ifdef erase
inline int UNDEF(erase)()  { return erase(); }
#undef erase
#define erase UNDEF(erase)
#endif

#ifdef flushok
inline int UNDEF(flushok)(WINDOW* _win, int _bf)  { return flushok(_win, _bf); }
#undef flushok
#define flushok UNDEF(flushok)
#else
#define _no_flushok
#endif

#ifdef getch
inline int UNDEF(getch)()  { return getch(); }
#undef getch
#define getch UNDEF(getch)
#endif

#ifdef getstr
inline int UNDEF(getstr)(char *_str)  { return getstr(_str); }
#undef getstr
#define getstr UNDEF(getstr)
#endif

#ifdef getyx
inline void UNDEF(getyx)(WINDOW* win, int& y, int& x) { getyx(win, y, x); }
#undef getyx
#define getyx UNDEF(getyx)
#endif

#ifdef getbegyx
inline void UNDEF(getbegyx)(WINDOW* win, int& y, int& x) { getbegyx(win, y, x); }
#undef getbegyx
#define getbegyx UNDEF(getbegyx)
#endif

#ifdef getmaxyx
inline void UNDEF(getmaxyx)(WINDOW* win, int& y, int& x) { getmaxyx(win, y, x); }
#undef getmaxyx
#define getmaxyx UNDEF(getmaxyx)
#endif

#ifdef hline
inline int UNDEF(hline)(chtype ch, int n) { return hline(ch, n); }
#undef hline
#define hline UNDEF(hline)
#endif

#ifdef inch
inline int UNDEF(inch)()  { return inch(); }
#undef inch
#define inch UNDEF(inch)
#endif

#ifdef insch
inline int UNDEF(insch)(char c)  { return insch(c); }
#undef insch
#define insch UNDEF(insch)
#endif

#ifdef insertln
inline int UNDEF(insertln)()  { return insertln(); }
#undef insertln
#define insertln UNDEF(insertln)
#endif

#ifdef leaveok
inline int UNDEF(leaveok)(WINDOW* win, int bf)  { return leaveok(win, bf); }
#undef leaveok
#define leaveok UNDEF(leaveok)
#else
extern "C" int leaveok(WINDOW* win, bool bf);
#endif

#ifdef move
inline int UNDEF(move)(int x, int y)  { return move(x, y); }
#undef move
#define move UNDEF(move)
#endif

#ifdef refresh
inline int UNDEF(refresh)()  { return refresh(); }
#undef refresh
#define refresh UNDEF(refresh)
#endif

#ifdef scrl
inline int UNDEF(scrl)(int l) { return scrl(l); }
#undef scrl
#define scrl UNDEF(scrl)
#endif

#ifdef scroll
inline int UNDEF(scroll)(WINDOW *win) { return scroll(win); }
#undef scroll
#define scroll UNDEF(scroll)
#endif

#ifdef scrollok
inline int UNDEF(scrollok)(WINDOW* win, int bf)  { return scrollok(win, bf); }
#undef scrollok
#define scrollok UNDEF(scrollok)
#else
#if	defined(__NCURSES_H)
extern "C" int scrollok(WINDOW*, bool);
#else
extern "C" int scrollok(WINDOW*, char);
#endif
#endif

#ifdef setscrreg
inline int UNDEF(setscrreg)(int t, int b) { return setscrreg(t, b); }
#undef setscrreg
#define setscrreg UNDEF(setscrreg)
#endif

#ifdef standend
inline int UNDEF(standend)()  { return standend(); }
#undef standend
#define standend UNDEF(standend)
#endif

#ifdef standout
inline int UNDEF(standout)()  { return standout(); }
#undef standout
#define standout UNDEF(standout)
#endif

#ifdef subpad
inline WINDOW *UNDEF(subpad)(WINDOW *p, int l, int c, int y, int x) 
{ return derwin(p,l,c,y,x); }
#undef subpad
#define subpad UNDEF(subpad)
#endif

#ifdef timeout
inline int UNDEF(timeout)(int delay) { return timeout(delay); }
#undef timeout
#define timeout UNDEF(timeout)
#endif

#ifdef touchline
inline int UNDEF(touchline)(WINDOW *win, int s, int c)
{ return touchline(win,s,c); }
#undef touchline
#define touchline UNDEF(touchline)
#endif

#ifdef touchwin
inline int UNDEF(touchwin)(WINDOW *win) { return touchwin(win); }
#undef touchwin
#define touchwin UNDEF(touchwin)
#endif

#ifdef untouchwin
inline int UNDEF(untouchwin)(WINDOW *win) { return untouchwin(win); }
#undef untouchwin
#define untouchwin UNDEF(untouchwin)
#endif

#ifdef vline
inline int UNDEF(vline)(chtype ch, int n) { return vline(ch, n); }
#undef vline
#define vline UNDEF(vline)
#endif

#ifdef waddstr
inline int UNDEF(waddstr)(WINDOW *win, char *str) { return waddstr(win, str); }
#undef waddstr
#define waddstr UNDEF(waddstr)
#endif

#ifdef waddchstr
inline int UNDEF(waddchstr)(WINDOW *win, chtype *at) { return waddchstr(win, at); }
#undef waddchstr
#define waddchstr UNDEF(waddchstr)
#endif

#ifdef wstandend
inline int UNDEF(wstandend)(WINDOW *win)  { return wstandend(win); }
#undef wstandend
#define wstandend UNDEF(wstandend)
#endif

#ifdef wstandout
inline int UNDEF(wstandout)(WINDOW *win)  { return wstandout(win); }
#undef wstandout
#define wstandout UNDEF(wstandout)
#endif

#ifdef wattroff
inline int UNDEF(wattroff)(WINDOW *win, int att) { return wattroff(win, att); }
#undef wattroff
#define wattroff UNDEF(wattroff)
#endif

#ifdef wattrset
inline int UNDEF(wattrset)(WINDOW *win, int att) { return wattrset(win, att); }
#undef wattrset
#define wattrset UNDEF(wattrset)
#endif

#ifdef winch
inline chtype UNDEF(winch)(WINDOW* win) { return winch(win); }
#undef winch
#define winch UNDEF(winch)
#endif

#ifdef mvwaddch
inline int UNDEF(mvwaddch)(WINDOW *win, int y, int x, chtype ch)
{ return mvwaddch(win, y, x, ch); }
#undef mvwaddch
#define mvwaddch UNDEF(mvwaddch)
#endif

#ifdef mvwaddchnstr
inline int UNDEF(mvwaddchnstr)(WINDOW *win, int y, int x, chtype *str, int n)
{ return mvwaddchnstr(win, y, x, str, n); }
#undef mvwaddchnstr
#define mvwaddchnstr UNDEF(mvwaddchnstr)
#endif

#ifdef mvwaddchstr
inline int UNDEF(mvwaddchstr)(WINDOW *win, int y, int x, chtype *str)
{ return mvwaddchstr(win, y, x, str); }
#undef mvwaddchstr
#define mvwaddchstr UNDEF(mvwaddchstr)
#endif

#ifdef mvwaddnstr
inline int UNDEF(mvwaddnstr)(WINDOW *win, int y, int x, const char *str, int n)
{ return mvwaddnstr(win, y, x, (char*)str, n); }
#undef mvwaddnstr
#define mvwaddnstr UNDEF(mvwaddnstr)
#endif

#ifdef mvwaddstr
inline int UNDEF(mvwaddstr)(WINDOW *win, int y, int x, const char * str)
{ return mvwaddstr(win, y, x, (char*)str); }
#undef mvwaddstr
#define mvwaddstr UNDEF(mvwaddstr)
#endif

#ifdef mvwdelch
inline int UNDEF(mvwdelch)(WINDOW *win, int y, int x)
{ return mvwdelch(win, y, x); }
#undef mvwdelch
#define mvwdelch UNDEF(mvwdelch)
#endif

#ifdef mvwgetch
inline int UNDEF(mvwgetch)(WINDOW *win, int y, int x) { return mvwgetch(win, y, x);}
#undef mvwgetch
#define mvwgetch UNDEF(mvwgetch)
#endif

#ifdef mvwgetstr
inline int UNDEF(mvwgetstr)(WINDOW *win, int y, int x, char *str)
{return mvwgetstr(win,y,x, str);}
#undef mvwgetstr
#define mvwgetstr UNDEF(mvwgetstr)
#endif

#ifdef mvwinch
inline int UNDEF(mvwinch)(WINDOW *win, int y, int x) { return mvwinch(win, y, x);}
#undef mvwinch
#define mvwinch UNDEF(mvwinch)
#endif

#ifdef mvwinsch
inline int UNDEF(mvwinsch)(WINDOW *win, int y, int x, char c)
{ return mvwinsch(win, y, x, c); }
#undef mvwinsch
#define mvwinsch UNDEF(mvwinsch)
#endif

#ifdef mvaddch
inline int UNDEF(mvaddch)(int y, int x, chtype ch)
{ return mvaddch(y, x, ch); }
#undef mvaddch
#define mvaddch UNDEF(mvaddch)
#endif

#ifdef mvaddnstr
inline int UNDEF(mvaddnstr)(int y, int x, const char *str, int n)
{ return mvaddnstr(y, x, (char*)str, n); }
#undef mvaddnstr
#define mvaddnstr UNDEF(mvaddnstr)
#endif

#ifdef mvaddstr
inline int UNDEF(mvaddstr)(int y, int x, const char * str)
{ return mvaddstr(y, x, (char*)str); }
#undef mvaddstr
#define mvaddstr UNDEF(mvaddstr)
#endif

#ifdef mvdelch
inline int UNDEF(mvdelch)(int y, int x) { return mvdelch(y, x);}
#undef mvdelch
#define mvdelch UNDEF(mvdelch)
#endif

#ifdef mvgetch
inline int UNDEF(mvgetch)(int y, int x) { return mvgetch(y, x);}
#undef mvgetch
#define mvgetch UNDEF(mvgetch)
#endif

#ifdef mvgetstr
inline int UNDEF(mvgetstr)(int y, int x, char *str) {return mvgetstr(y, x, str);}
#undef mvgetstr
#define mvgetstr UNDEF(mvgetstr)
#endif

#ifdef mvinch
inline int UNDEF(mvinch)(int y, int x) { return mvinch(y, x);}
#undef mvinch
#define mvinch UNDEF(mvinch)
#endif

#ifdef mvinsch
inline int UNDEF(mvinsch)(int y, int x, char c)
{ return mvinsch(y, x, c); }
#undef mvinsch
#define mvinsch UNDEF(mvinsch)
#endif

#ifdef napms
inline void UNDEF(napms)(unsigned long x) { napms(x); }
#undef napms
#define napms UNDEF(napms)
#endif

#ifdef fixterm
inline int UNDEF(fixterm)(void) { return fixterm(); }
#undef fixterm
#define fixterm UNDEF(fixterm)
#endif

#ifdef resetterm
inline int UNDEF(resetterm)(void) { return resetterm(); }
#undef resetterm
#define resetterm UNDEF(resetterm)
#endif

#ifdef saveterm
inline int UNDEF(saveterm)(void) { return saveterm(); }
#undef saveterm
#define saveterm UNDEF(saveterm)
#endif

#ifdef crmode
inline int UNDEF(crmode)(void) { return crmode(); }
#undef crmode
#define crmode UNDEF(crmode)
#endif

#ifdef nocrmode
inline int UNDEF(nocrmode)(void) { return nocrmode(); }
#undef nocrmode
#define nocrmode UNDEF(nocrmode)
#endif

/*
 *
 * C++ class for windows.
 *
 *
 */

class NCursesWindow
{
  friend class NCursesMenu; friend class NCursesForm;
  
private:
  static void    initialize();
  void           init(); 
  void           err_handler(const char *) const THROWS(NCursesException);

  short          getcolor(int getback) const;

  static int     setpalette(short fore, short back, short pair);
  static int     colorInitialized;
  
protected:
  static int     count;            // count of all active windows:
  //   We rely on the c++ promise that
  //   all otherwise uninitialized
  //   static class vars are set to 0
  
  WINDOW *       w;                // the curses WINDOW
  
  int            alloced;          // true if we own the WINDOW
  
  NCursesWindow* par;              // parent, if subwindow
  NCursesWindow* subwins;          // head of subwindows list
  NCursesWindow* sib;              // next subwindow of parent
  
  void           kill_subwindows(); // disable all subwindows
  
public:
  NCursesWindow(WINDOW* &window);  // useful only for stdscr
  
  NCursesWindow(int lines,         // number of lines
		int cols,          // number of columns
		int begin_y,       // line origin
		int begin_x);      // col origin
  
  NCursesWindow(NCursesWindow& par,// parent window
		int lines,         // number of lines
		int cols,          // number of columns
		int by,            // absolute or relative
		int bx,            //   origins:
		char absrel = 'a');// if `a', by & bx are
  // absolute screen pos,
  // else if `r', they are
  // relative to par origin
  virtual ~NCursesWindow();
  
  static void    useColors(void);
  // Call this routine very early if you want to have colors.

  // terminal status
  int            lines() const { return LINES; }
  // number of lines on terminal, *not* window
  int            cols() const { return COLS; }
  // number of cols  on terminal, *not* window
  static int     NumberOfColors();
  // number of available colors
  int     colors() const { return NumberOfColors(); }
  // number of available colors
  
  // window status
  int            height() const { return maxy() + 1; }
  // number of lines in this window
  int            width() const { return maxx() + 1; }
  // number of cols in this window
  int            begx() const { return w->_begx; }
  // smallest x coord in window
  int            begy() const { return w->_begy; }
  // smallest y coord in window
  int            maxx() const { return w->_maxx; }
  // largest  x coord in window
  int            maxy() const { return w->_maxy; }
  // largest  x coord in window
  short  getcolor() const;
  // actual color pair
  short  foreground() const { return getcolor(0); }
  // actual foreground color
  short  background() const { return getcolor(1); }
  // actual background color
  int    setpalette(short fore, short back);
  // set color palette entry
  int    setcolor(short pair);
  // set actually used palette entry
  
  // window positioning
  int            move(int y, int x) { return ::wmove(w, y, x); }
  
  // coordinate positioning
  void           getyx(int& y, int& x) { ::getyx(w, y, x); }
  int            mvcur(int sy, int ey, int sx, int ex) { 
    return ::mvcur(sy, ey, sx, ex); }
  
  // input
  int            getch() { return ::wgetch(w); }
  int            getch(int y, int x) {
    return (::wmove(w, y, x)==ERR) ? ERR : ::wgetch(w); }
  int            getstr(char* str) { return ::wgetstr(w, str); }
  int            getstr(int y, int x, char* str) { 
    return (::wmove(w, y, x)==ERR) ? ERR : ::wgetstr(w, str); }
  int            scanw(const char*, ...)
#if __GNUG__ >= 2
    __attribute__ ((format (scanf, 2, 3)));
#else
  ;
#endif
  int            scanw(int, int, const char*, ...)
#if __GNUG__ >= 2
    __attribute__ ((format (scanf, 4, 5)));
#else
  ;
#endif
  
  // output
  int            addch(const chtype ch) { return ::waddch(w, ch); }
  int            addch(int y, int x, chtype ch) {
    return (::wmove(w, y, x)==ERR) ? ERR : ::waddch(w, ch); }
  int            addstr(const char* str) { return ::waddstr(w, (char*)str); }
  int            addstr(int y, int x, const char * str) {
    return (::wmove(w, y, x)==ERR) ? ERR : ::waddstr(w, (char*)str); }
  int            printw(const char* fmt, ...)
#if __GNUG__ >= 2
    __attribute__ ((format (printf, 2, 3)));
#else
  ;
#endif
  int            printw(int y, int x, const char * fmt, ...)
#if __GNUG__ >= 2
    __attribute__ ((format (printf, 4, 5)));
#else
  ;
#endif
  int            inch() { return ::winch(w); }
  int            inch(int y, int x) {
    return (::wmove(w, y, x)==ERR) ? ERR : (int) ::winch(w); }
  int            insch(chtype ch) { return ::winsch(w, ch); }
  int            insch(int y, int x, chtype ch) {
    return (::wmove(w, y, x)==ERR) ? ERR : ::winsch(w, ch); }
  int            insertln() { return ::winsertln(w); }
  int            attron(chtype at) { return ::wattron(w, at); }
  int            attroff(chtype at) { return ::wattroff(w, at); }
  int            attrset(chtype at) { return ::wattrset(w, at); }
  
  // borders
  int            box(chtype vert=0, chtype  hor=0) { return ::box(w, vert, hor); }
  
  // lines and boxes
  int            hline(int y, int x, chtype ch, int len) {
    return (::wmove(w, y, x)==ERR) ? ERR: ::whline(w, ch, len); }
  int            hline(int y, int x, int len) {
    return (::wmove(w, y, x)==ERR) ? ERR: ::whline(w, 0, len); }
  int            hline(chtype ch, int len) { return ::whline(w, ch, len); }
  int            hline(int len) { return ::whline(w, 0, len); }
  int            vline(int y, int x, chtype ch, int len) {
    return (::wmove(w, y, x)==ERR) ? ERR: ::wvline(w, ch, len); }
  int            vline(int y, int x, int len) {
    return (::wmove(w, y, x)==ERR) ? ERR: ::wvline(w, 0, len); }
  int            vline(chtype ch, int len) { return ::wvline(w, ch, len); }
  int            vline(int len) { return ::wvline(w, 0, len); }
  
  // erasure
  int            erase() { return ::werase(w); }
  int            clear() { return ::wclear(w); }
  int            clearok(int bf) { return ::clearok(w, bf); }
  int            clrtobot() { return ::wclrtobot(w); }
  int            clrtoeol() { return ::wclrtoeol(w); }
  int            delch() { return ::wdelch(w); }
  int            delch(int y, int x) {
    return (::wmove(w, y, x)==ERR) ? ERR : ::wdelch(w); }
  int            deleteln() { return ::wdeleteln(w); }
  
  // screen control
  int            scroll() { return ::scroll(w); }
  int            scrollok(int bf) { return ::scrollok(w, bf); }
  int            idlok(int bf) { return ::idlok(w, bf); }
  int            touchwin() { return ::touchwin(w); }
  int            refresh() { return ::wrefresh(w); }
  int            leaveok(int bf) { return ::leaveok(w, bf); }
  int            noutrefresh() { return ::wnoutrefresh(w); }
  int            doupdate() { return ::doupdate(); }
#ifndef _no_flushok
  int            flushok(int bf) { return ::flushok(w, bf); }
#endif
  int            keypad(int bf) { return ::keypad(w, bf); }
  int            standout() { return ::wstandout(w); }
  int            standend() { return ::wstandend(w); }
  
  // multiple window control
  int            overlay(NCursesWindow &win) {
    return ::overlay(w, win.w); }
  int            overwrite(NCursesWindow &win) {
    return ::overwrite(w, win.w); }
  
  
  // traversal support
  NCursesWindow*  child() { return subwins; }
  NCursesWindow*  sibling() { return sib; }
  NCursesWindow*  parent() { return par; }
  
  // True if win is a child of this window. 
  bool isDescendant(NCursesWindow& win);
};


// We leave this here for compatibility reasons.
class NCursesColorWindow : public NCursesWindow {
public:
  NCursesColorWindow(WINDOW* &window)   // useful only for stdscr
    : NCursesWindow(window) {
      useColors(); }

  NCursesColorWindow(int lines,         // number of lines
		     int cols,          // number of columns
		     int begin_y,       // line origin
		     int begin_x)       // col origin
    : NCursesWindow(lines,cols,begin_y,begin_x) {
      useColors(); }

  NCursesColorWindow(NCursesWindow& par,// parent window
		     int lines,         // number of lines
		     int cols,          // number of columns
		     int by,            // absolute or relative
		     int bx,            //   origins:
		     char absrel = 'a') // if `a', by & bx are
    : NCursesWindow(par,lines,cols,     // absolute screen pos,
		    by,bx) {            // else if `r', they are
      useColors(); }                    // relative to par origin  
};

#endif // _CURSESW_H
