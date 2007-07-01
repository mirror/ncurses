/*
 * $Id: test_getstr.c,v 1.1 2007/06/30 16:51:28 tom Exp $
 *
 * Demonstrate the getstr functions from the curses library.

       int getstr(char *str);
       int getnstr(char *str, int n);
       int wgetstr(WINDOW *win, char *str);
       int wgetnstr(WINDOW *win, char *str, int n);
       int mvgetstr(int y, int x, char *str);
       int mvwgetstr(WINDOW *win, int y, int x, char *str);
       int mvgetnstr(int y, int x, char *str, int n);
       int mvwgetnstr(WINDOW *, int y, int x, char *str, int n);
 */

#include <test.priv.h>

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    printf("Not implemented - test-driver for curses getstr() functions\n");
    return EXIT_SUCCESS;
}
