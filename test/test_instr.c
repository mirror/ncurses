/*
 * $Id: test_instr.c,v 1.1 2007/06/30 16:51:44 tom Exp $
 *
 * Demonstrate the instr functions from the curses library.

       int instr(char *str);
       int innstr(char *str, int n);
       int winstr(WINDOW *win, char *str);
       int winnstr(WINDOW *win, char *str, int n);
       int mvinstr(int y, int x, char *str);
       int mvinnstr(int y, int x, char *str, int n);
       int mvwinstr(WINDOW *win, int y, int x, char *str);
       int mvwinnstr(WINDOW *win, int y, int x, char *str, int n);
 */

#include <test.priv.h>

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    printf("Not implemented - test-driver for curses instr() functions\n");
    return EXIT_SUCCESS;
}
