/*
 *
 * This is a test program for the PDCurses screen package for IBM PC type
 * machines.
 * This program was written by John Burnell (johnb@kea.am.dsir.govt.nz)
 * esr changed the usleep calls to napms calls, 7 Nov 1995
 *
 * $Id: testcurs.c,v 1.14 1997/04/06 01:44:16 tom Exp $
 */

#include <test.priv.h>

static void display_menu (int,int);
static int  initTest (WINDOW **);
static void inputTest (WINDOW *);
static void introTest (WINDOW *);
static void outputTest (WINDOW *);
static void padTest (WINDOW *);
#ifdef __PDCURSES__
static void resizeTest (WINDOW *);
#endif
static void scrollTest (WINDOW *);

struct commands
{
 NCURSES_CONST char *text;
 void (*function)(WINDOW *);
};
typedef struct commands COMMAND;

const COMMAND command[] =
{
 {"Intro Test",  introTest},
 {"Pad Test",    padTest},
#ifdef __PDCURSES__
 {"Resize Test", resizeTest},
#endif
 {"Scroll Test", scrollTest},
 {"Input Test",  inputTest},
 {"Output Test", outputTest}
};
#define MAX_OPTIONS ((sizeof(command)/sizeof(command[0])))

int     width, height;

int
main(
	int argc GCC_UNUSED,
	char *argv[] GCC_UNUSED)
{
WINDOW  *win;
int key,old_option=(-1),new_option=0;
bool quit=FALSE;

#ifdef PDCDEBUG
	PDC_debug("testcurs started\n");
#endif
    if (!initTest (&win)) return EXIT_FAILURE;

#ifdef A_COLOR
    if (has_colors())
      {
       init_pair(1,COLOR_WHITE,COLOR_BLUE);
       wattrset(win, COLOR_PAIR(1));
      }
    else
       wattrset(win, A_REVERSE);
#else
    wattrset(win, A_REVERSE);
#endif

    erase();
    display_menu(old_option,new_option);
    while(1)
      {
       noecho();
       keypad(stdscr,TRUE);
       raw();
       key = getch();
       switch(key)
         {
          case 10:
          case 13:
          case KEY_ENTER:
                         erase();
                         refresh();
                         (*command[new_option].function)(win);
                         erase();
                         display_menu(old_option,new_option);
                         break;
          case KEY_UP:
                         new_option = (new_option == 0) ? new_option : new_option-1;
                         display_menu(old_option,new_option);
                         break;
          case KEY_DOWN:
                         new_option = (new_option == MAX_OPTIONS-1) ? new_option : new_option+1;
                         display_menu(old_option,new_option);
                         break;
          case 'Q':
          case 'q':
                         quit = TRUE;
                         break;
          default:       break;
         }
       if (quit == TRUE)
          break;
      }

    delwin (win);

    endwin();
    return EXIT_SUCCESS;
}

static
void Continue (WINDOW *win)
{
    wmove(win, 10, 1);
/*    wclrtoeol(win);
*/    mvwaddstr(win, 10, 1, " Press any key to continue");
    wrefresh(win);
    raw();
    wgetch(win);
}

static
int initTest (WINDOW **win)
{
#ifdef PDCDEBUG
	PDC_debug("initTest called\n");
#endif
#ifdef NCURSES_VERSION
	trace(TRACE_MAXIMUM);
#endif
    initscr();
#ifdef PDCDEBUG
	PDC_debug("after initscr()\n");
#endif
#ifdef A_COLOR
    if (has_colors())
       start_color();
#endif
    width  = 60;
    height = 13;                /* Create a drawing window */
    *win = newwin(height, width, (LINES-height)/2, (COLS-width)/2);
    if(*win == NULL)
    {   endwin();
        return 0;
    }
    return 1;
}

static void
introTest (WINDOW *win)
{
    beep ();
    werase(win);

    box(win, ACS_VLINE, ACS_HLINE);
    wrefresh(win);
    cbreak ();
    mvwaddstr(win, 1, 1, "You should have rectangle in the middle of the screen");
    mvwaddstr(win, 2, 1, "You should have heard a beep");
    Continue(win);
    return;
}

static void
scrollTest (WINDOW *win)
{
    int i;
    int OldX, OldY;
    const char *Message = "The window will now scroll slowly";

    wclear(win);
    mvwprintw (win, height - 2, 1, Message);
    wrefresh (win);
    scrollok(win, TRUE);
    for (i = 1; i <= height; i++) {
      napms(250);
      scroll(win);
      wrefresh (win);
    };

    getmaxyx (win, OldY, OldX);
    mvwprintw (win, 6, 1, "The top of the window will scroll");
    wmove (win, 1, 1);
    wsetscrreg (win, 0, 4);
    box(win, ACS_VLINE, ACS_HLINE);
    wrefresh (win);
    for (i = 1; i <= 5; i++) {
      napms(500);
      scroll(win);
      wrefresh (win);
    };
    wsetscrreg (win, 0, --OldY);

}

static void
inputTest (WINDOW *win)
{
    int w, h, bx, by, sw, sh, i, c,num;
    char buffer [80];
    WINDOW *subWin;
    wclear (win);

    w  = win->_maxx;
    h  = win->_maxy;
    bx = win->_begx;
    by = win->_begy;
    sw = w / 3;
    sh = h / 3;
    if((subWin = subwin(win, sh, sw, by + h - sh - 2, bx + w - sw - 2)) == NULL)
        return;

#ifdef A_COLOR
    if (has_colors())
      {
       init_pair(2,COLOR_CYAN,COLOR_BLUE);
       wattrset(subWin, COLOR_PAIR(2) | A_BOLD);
      }
    else
       wattrset(subWin, A_BOLD);
#else
    wattrset(subWin, A_BOLD);
#endif
    box(subWin, ACS_VLINE, ACS_HLINE);
    wrefresh(win);

    nocbreak();
    mvwaddstr(win, 2, 1, "Press some keys for 5 seconds");
    mvwaddstr(win, 1, 1, "Pressing ^C should do nothing");
    wrefresh(win);

    for (i = 0; i < 5; i++) {
      werase (subWin);
      box(subWin, ACS_VLINE, ACS_HLINE);
      mvwprintw (subWin, 1, 1, "Time = %d", i);
      wrefresh(subWin);
      napms(1000);
      flushinp();
    }

    delwin (subWin);
    werase(win);
    flash();
    wrefresh(win);
    napms(500);

    mvwaddstr(win, 2, 1, "Press a key, followed by ENTER");
    wmove(win, 9, 10);
    wrefresh(win);
    echo();
    noraw();
    wgetch(win);
    flushinp();

    wmove(win, 9, 10);
    wdelch(win);
    mvwaddstr(win, 4, 1, "The character should now have been deleted");
    Continue(win);

    wclear (win);
    mvwaddstr(win, 2, 1, "Press a function key or an arrow key");
    wrefresh(win);
    keypad(win, TRUE);
    raw();
    c = wgetch(win);

    nodelay(win, TRUE);
    wgetch(win);
    nodelay(win, FALSE);

    refresh();
    wclear (win);
    mvwaddstr(win, 3, 2, "The window should have moved");
    mvwaddstr(win, 4, 2, "This text should have appeared without you pressing a key");
    mvwprintw(win, 2, 2, "Keycode = %d", c);
    mvwaddstr(win, 6, 2, "Enter a number then a string seperated by space");
    echo();
    noraw();
    mvwscanw(win, 7, 6, "%d %s", &num,buffer);
    mvwprintw(win, 8, 6, "String: %s Number: %d", buffer,num);
    Continue(win);
}

static void
outputTest (WINDOW *win)
{
    WINDOW *win1;
    char Buffer [80];
    chtype ch;

    nl ();
    wclear (win);
    mvwaddstr(win, 1, 1, "You should now have a screen in the upper left corner, and this text should have wrapped");
    mvwin(win, 2, 1);
    Continue(win);

    wclear(win);
    mvwaddstr(win, 1, 1, "A new window will appear with this text in it");
    mvwaddstr(win, 8, 1, "Press any key to continue");
    wrefresh(win);
    wgetch(win);

    win1 = newwin(10, 50, 15, 25);
    if(win1 == NULL)
    {   endwin();
        return;
    }
#ifdef A_COLOR
    if (has_colors())
      {
       init_pair(3,COLOR_BLUE,COLOR_WHITE);
       wattrset(win1, COLOR_PAIR(3));
      }
    else
       wattrset(win1, A_NORMAL);
#else
    wattrset(win1, A_NORMAL);
#endif
    wclear (win1);
    mvwaddstr(win1, 5, 1, "This text should appear; using overlay option");
    copywin(win, win1,0,0,0,0,10,50,TRUE);

    box(win1,ACS_VLINE,ACS_HLINE);

    wmove(win1, 8, 26);
    wrefresh(win1);
    wgetch(win1);

    wclear(win1);
    wattron(win1, A_BLINK);
    mvwaddstr(win1, 4, 1, "This blinking text should appear in only the second window");
    wattroff(win1, A_BLINK);
    wrefresh(win1);
    wgetch(win1);
    delwin(win1);

    wclear(win);
    wrefresh(win);
    mvwaddstr(win, 6, 2, "This line shouldn't appear");
    mvwaddstr(win, 4, 2, "Only half of the next line is visible");
    mvwaddstr(win, 5, 2, "Only half of the next line is visible");
    wmove(win, 6, 1);
    wclrtobot (win);
    wmove(win, 5, 20);
    wclrtoeol (win);
    mvwaddstr(win, 8, 2, "This line also shouldn't appear");
    wmove(win, 8, 1);
    wdeleteln(win);
    Continue(win);

    wmove (win, 5, 9);
    ch = winch (win);

    wclear(win);
    wmove (win, 6, 2);
    waddstr (win, "The next char should be l:  ");
    winsch (win, ch);
    Continue(win);

    wmove(win, 5, 1);
    winsertln (win);
    mvwaddstr(win, 5, 2, "The lines below should have moved down");
    Continue(win);

    wclear(win);
    wmove(win, 2, 2);
    wprintw(win, "This is a formatted string in a window: %d %s\n", 42, "is it");
    mvwaddstr(win, 10, 1, "Enter a string: ");
    wrefresh(win);
    noraw();
    echo();
    wscanw (win, "%s", Buffer);

    wclear(win);
    mvwaddstr(win, 10, 1, "Enter a string");
    wrefresh(win);
    clear();
    move(0,0);
    printw("This is a formatted string in stdscr: %d %s\n", 42, "is it");
    mvaddstr(10, 1, "Enter a string: ");
    refresh();
    noraw();
    echo();
    scanw ("%s", Buffer);

    wclear(win);
    curs_set(2);
    mvwaddstr(win, 1, 1, "The cursor should appear as a block (visible)");
    Continue(win);

    wclear(win);
    curs_set(0);
    mvwaddstr(win, 1, 1, "The cursor should have disappeared (invisible)");
    Continue(win);

    wclear(win);
    curs_set(1);
    mvwaddstr(win, 1, 1, "The cursor should be an underline (normal)");
    Continue(win);
}

#ifdef __PDCURSES__
static void
resizeTest(WINDOW *dummy GCC_UNUSED)
{
    WINDOW *win1;

    savetty ();

    clear();
    refresh();
    resize(50);


    win1 = newwin(11, 50, 14, 25);
    if(win1 == NULL)
    {   endwin();
        return;
    }
#ifdef A_COLOR
    if (has_colors())
      {
       init_pair(3,COLOR_BLUE,COLOR_WHITE);
       wattrset(win1, COLOR_PAIR(3));
      }
#endif
    wclear (win1);

    mvwaddstr(win1, 1, 1, "The screen may now have 50 lines");
    Continue(win1);

    resetty ();

    wclear (win1);
    mvwaddstr(win1, 1, 1, "The screen should now be reset");
    Continue(win1);

    delwin(win1);

    clear();
    refresh();

}
#endif

static void
padTest(WINDOW *dummy GCC_UNUSED)
{
WINDOW *pad;

 pad = newpad(50,100);
 mvwaddstr(pad, 5, 2, "This is a new pad");
 mvwaddstr(pad, 8, 0, "The end of this line should be truncated here:abcd");
 mvwaddstr(pad,11, 1, "This line should not appear.");
 wmove(pad, 10, 1);
 wclrtoeol(pad);
 mvwaddstr(pad, 10, 1, " Press any key to continue");
 prefresh(pad,0,0,0,0,10,45);
 keypad(pad, TRUE);
 raw();
 wgetch(pad);

 mvwaddstr(pad, 35, 2, "This is displayed at line 35 in the pad");
 mvwaddstr(pad, 40, 1, " Press any key to continue");
 prefresh(pad,30,0,0,0,10,45);
 keypad(pad, TRUE);
 raw();
 wgetch(pad);

 delwin(pad);
}

static void
display_menu(int old_option,int new_option)
{
 register size_t i;

 attrset(A_NORMAL);
 mvaddstr(3,20,"PDCurses Test Program");

 for (i=0;i<MAX_OPTIONS;i++)
    mvaddstr(5+i,25,command[i].text);
 if (old_option != (-1))
    mvaddstr(5+old_option,25,command[old_option].text);
 attrset(A_REVERSE);
 mvaddstr(5+new_option,25,command[new_option].text);
 attrset(A_NORMAL);
 mvaddstr(13,3,"Use Up and Down Arrows to select - Enter to run - Q to quit");
 refresh();
}

