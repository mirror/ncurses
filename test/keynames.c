/*
 * $Id: keynames.c,v 1.4 2002/09/01 19:42:42 tom Exp $
 */

#include <test.priv.h>

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    int n;
    for (n = -1; n < 512; n++) {
	char *result = keyname(n);
	if (result != 0)
	    printf("%d(%5o):%s\n", n, n, result);
    }
    ExitProgram(EXIT_SUCCESS);
}
