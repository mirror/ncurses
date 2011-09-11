#!/bin/sh
##############################################################################
# Copyright (c) 2004,2011 Free Software Foundation, Inc.                     #
#                                                                            #
# Permission is hereby granted, free of charge, to any person obtaining a    #
# copy of this software and associated documentation files (the "Software"), #
# to deal in the Software without restriction, including without limitation  #
# the rights to use, copy, modify, merge, publish, distribute, distribute    #
# with modifications, sublicense, and/or sell copies of the Software, and to #
# permit persons to whom the Software is furnished to do so, subject to the  #
# following conditions:                                                      #
#                                                                            #
# The above copyright notice and this permission notice shall be included in #
# all copies or substantial portions of the Software.                        #
#                                                                            #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    #
# THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER      #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        #
# DEALINGS IN THE SOFTWARE.                                                  #
#                                                                            #
# Except as contained in this notice, the name(s) of the above copyright     #
# holders shall not be used in advertising or otherwise to promote the sale, #
# use or other dealings in this Software without prior written               #
# authorization.                                                             #
##############################################################################
#
# Author: Thomas E. Dickey
#
# $Id: gen_edit.sh,v 1.2 2011/09/11 00:47:26 tom Exp $
# Generate a sed-script for converting the terminfo.src to the form which will
# be installed.
#
# Assumes:
#	The leaf directory names (lib, tabset, terminfo)
#

: ${ticdir=@TERMINFO@}
: ${xterm_new=@WHICH_XTERM@}

# If we're not installing into /usr/share/, we'll have to adjust the location
# of the tabset files in terminfo.src (which are in a parallel directory).
TABSET=`echo $ticdir | sed -e 's%/terminfo$%/tabset%'`
if test "x$TABSET" != "x/usr/share/tabset" ; then
cat <<EOF
s%/usr/share/tabset%$TABSET%g
EOF
fi

if test "$xterm_new" != "xterm-new" ; then
cat <<EOF
/^# This is xterm for ncurses/,/^$/{
	s/use=xterm-new,/use=$WHICH_XTERM,/
}
EOF
fi

# Work around incompatibities built into Linux console.  The 2.6 series added
# a patch to fixup the SI/SO behavior, which is closer to vt100, but the older
# kernels do not recognize those controls.
system=`uname -s 2>/dev/null`
if test "x$system" = xLinux
then
	case x`uname -r` in
	x1.*)
cat <<EOF
/^# This is Linux console for ncurses/,/^$/{
	s/use=linux3.0,/use=linux-c,/
}
EOF
		;;
	x2.[0-4]*)
cat <<EOF
/^# This is Linux console for ncurses/,/^$/{
	s/use=linux3.0,/use=linux2.2,/
}
EOF
		;;
	esac
fi
