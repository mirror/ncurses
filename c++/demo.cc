/*
 *   Silly demo program for the NCursesPanel class.
 *
 *   written by Anatoly Ivasyuk (anatoly@nick.csh.rit.edu)
 *
 * $Id: demo.cc,v 1.7 1997/05/05 20:53:41 tom Exp $
 */

#include <stdlib.h>

#include "cursesm.h"

#if HAVE_LIBC_H
#  include <libc.h>
#endif

class SillyDemo 
{
  public:
  void run(int sleeptime) {
    //  We need to define a full screen panel for the main screen so
    //  that when redraws happen, the main screen actually gets redrawn.
    //  I think there may be a bug in the panels code which won't redraw
    //  the main screen otherwise.  Maybe someone out there can find it...

    NCursesPanel *std = new NCursesPanel();

    //  Make a few small demo panels

    NCursesPanel *u = new NCursesPanel(10,20,12,4);
    NCursesPanel *v = new NCursesPanel(10,20,10,6);
    NCursesPanel *w = new NCursesPanel(10,20,8,8);
    NCursesPanel *x = new NCursesPanel(10,20,6,10);
    NCursesPanel *y = new NCursesPanel(10,20,4,12);
    NCursesPanel *z = new NCursesPanel(10,30,2,14);

    //  Draw something on the main screen, so we can see what happens
    //  when panels get moved or deleted.

    std->box();
    std->move(10,0);
    std->hline('a',79);
    std->move(0,40);
    std->vline(20);

    //  Draw frames with titles around panels so that we can see where
    //  the panels are located.

    u->boldframe("Win U");
    v->frame("Win V");
    w->boldframe("Win W");
    x->frame("Win X");
    y->boldframe("Win Y");
    z->frame("Win Z");

    //  A refresh to any valid panel updates all panels and refreshes
    //  the screen.  Using std is just convenient - We know it's always
    //  valid until the end of the program.

    std->refresh();

    //  Show that things actually come back correctly when the screen
    //  is cleared and the global NCursesPanel::redraw() is called.

    sleep(sleeptime);
    ::clear();			// call ncurses clear() directly
    ::wrefresh(stdscr);		// call ncurses refresh directly
    sleep(sleeptime);
    NCursesPanel::redraw();

    //  Show what happens when panels are deleted and moved.

    sleep(sleeptime);
    delete u;
    std->refresh();

    sleep(sleeptime);
    delete z;
    std->refresh();

    sleep(sleeptime);
    delete v;
    std->refresh();

    // show how it looks when a panel moves
    sleep(sleeptime);
    y->mvpan(5,30);
    std->refresh();

    sleep(sleeptime);
    delete y;
    std->refresh();

    // show how it looks when you raise a panel
    sleep(sleeptime);
    w->top();
    std->refresh();

    sleep(sleeptime);
    delete w;
    std->refresh();

    sleep(sleeptime);
    delete x;
    std->refresh();

    //  Don't forget to clean up the main screen.  Since this is the
    //  last thing using NCursesWindow, this has the effect of
    //  shutting down ncurses and restoring the terminal state.

    sleep(sleeptime);
    delete std;
  }
};


class UserData
{
private:
  int u;
public:
  UserData(int x) : u(x) {} 
  int sleeptime() const { return u; }
  
};

template<class T> class MyAction : public NCursesUserItem<T>
{
public:
  MyAction (const T* p_UserData,
	    const char* p_name)
    : NCursesUserItem<T>(p_UserData, p_name)
  {};

  ~MyAction() {}

  bool action() {
    SillyDemo a;
    a.run(UserData()->sleeptime());
    return FALSE;
  }
};

class QuitItem : public NCursesMenuItem
{
public:
  QuitItem() : NCursesMenuItem("Quit") {
  }

  bool action() {
    endwin();
    return TRUE;
  }
};

class MyMenu : public NCursesMenu
{
private:
  NCursesPanel* P;

public:
  MyMenu (NCursesMenuItem* menu[]) 
    : NCursesMenu (menu, 7, 8, 2, 2, TRUE)
  {
    if (NCursesWindow::NumberOfColors() > 2) {
      setcolor(1);
      setpalette(COLOR_YELLOW, COLOR_BLUE);
    }

    P = new NCursesPanel(1,COLS,LINES-1,0);
    boldframe("Demo","Silly");
    P->show();
  }

  ~MyMenu()
  {
    P->hide();
    delete P;
  }

  virtual void On_Menu_Init()
  {
    P->move(0,0);
    P->clrtoeol();
    P->addstr("12345");
    NCursesPanel::refresh();
  }

  virtual void On_Menu_Termination()
  {
    P->move(0,0);
    P->clrtoeol();
    P->addstr("Menu Exit");
    NCursesPanel::refresh();
  }

  virtual void On_Item_Init(NCursesMenuItem& item)
  {
    P->move(0,item.index());
    P->attron(A_REVERSE);
    P->printw("%1d",1+item.index());
    P->attroff(A_REVERSE);
    NCursesPanel::refresh();
  }

  virtual void On_Item_Termination(NCursesMenuItem& item)
  {
    P->move(0,item.index());
    P->attroff(A_REVERSE);
    P->printw("%1d",1+item.index());
    NCursesPanel::refresh();
  }
};

main()
{
  UserData* u = new UserData(1);

  NCursesWindow::useColors();

  NCursesMenuItem** I = new NCursesMenuItem*[6];
  I[0] = new NCursesMenuItem("One");
  I[1] = new NCursesMenuItem("Two");
  I[2] = new MyAction<UserData> (u, "Silly");
  I[3] = new NCursesMenuItem("Four");
  I[4] = new QuitItem();
  I[5] = new NCursesMenuItem();
  
  MyMenu m(I);

  m();

  for(int i=0; i < 6; i++) {
    delete I[i];
  }
  delete I;
  delete u;

  exit(0);
}
