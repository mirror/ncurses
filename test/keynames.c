/*
 * $Id: keynames.c,v 1.1 1997/10/25 18:41:04 tom Exp $
 */

#include <test.priv.h>

int main(int argc, char *argv[])
{
	int n;
	for (n = -1; n < 512; n++) {
		printf("%d(%5o):%s\n", n, n, keyname(n));
	}
	return EXIT_SUCCESS;
}
