#!/bin/sh
################################################################################
# Copyright 1996 by Thomas E. Dickey <dickey@clark.net>                        #
# All Rights Reserved.                                                         #
#                                                                              #
# Permission to use, copy, modify, and distribute this software and its        #
# documentation for any purpose and without fee is hereby granted, provided    #
# that the above copyright notice appear in all copies and that both that      #
# copyright notice and this permission notice appear in supporting             #
# documentation, and that the name of the above listed copyright holder(s) not #
# be used in advertising or publicity pertaining to distribution of the        #
# software without specific, written prior permission. THE ABOVE LISTED        #
# COPYRIGHT HOLDER(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,    #
# INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT #
# SHALL THE ABOVE LISTED COPYRIGHT HOLDER(S) BE LIABLE FOR ANY SPECIAL,        #
# INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM   #
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE   #
# OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR    #
# PERFORMANCE OF THIS SOFTWARE.                                                #
################################################################################
# $Id: run_tic.sh,v 1.8 1997/05/09 10:43:29 hjl Exp $
# This script is used to install terminfo.src using tic.  We use a script
# because the path checking is too awkward to do in a makefile.
#
# Parameters:
#	$1 = the common object directory.
#	$2 = source-directory, i.e., $(srcdir)
#	$3 = destination-directory path, i.e., $(ticdir)
#	$4 = install-prefix, if any
#
# Assumes:
#	The leaf directory names (bin, lib, shared, tabset, terminfo)
#
echo '** Building terminfo database, please wait...'
#
# Parameter parsing is primarily for debugging.  The script is designed to
# be run from the misc/Makefile as
#	make install.data

prefix=/usr
if test $# != 0 ; then
	common_objpfx=$1
	shift
fi

if test $# != 0 ; then
	srcdir=$1
	shift
else
	srcdir=.
fi

if test $# != 0 ; then
	ticdir=$1
	shift
else
	ticdir=$prefix/share/terminfo
fi

if test $# != 0 ; then
	IP=$1
	shift
else
	IP=""
fi

TERMINFO=$IP$ticdir ; export TERMINFO
umask 022

# Construct the name of the old (obsolete) pathname, e.g., /usr/lib/terminfo.
TICDIR=`echo $TERMINFO | sed -e 's/\/share\//\/lib\//'`

# Remove the old terminfo stuff; we don't care if it existed before, and it
# would generate a lot of confusing error messages if we tried to overwrite it.
# We explicitly remove its contents rather than the directory itself, in case
# the directory is actually a symbolic link.
( rm -fr $TERMINFO/[0-9A-Za-z] 2>/dev/null )

# If we're not installing into /usr/share/, we'll have to adjust the location
# of the tabset files in terminfo.src (which are in a parallel directory).
TABSET=`echo $ticdir | sed -e 's/\/terminfo$/\/tabset/'`
SRC=$srcdir/terminfo.src
if test "x$TABSET" != "x/usr/share/tabset" ; then
	echo '** adjusting tabset paths'
	TMP=${TMPDIR-/tmp}/$$
	sed -e s:/usr/share/tabset:$TABSET:g $SRC >$TMP
	trap "rm -f $TMP" 0 1 2 5 15
	SRC=$TMP
fi

LD_LIBRARY_PATH=$common_objpfx:$common_objpfx/nss:$common_objpfx/ncurses \
	$common_objpfx/elf/ld.so \
	$common_objpfx/ncurses/tic -s $SRC
if [ $? = 0 ]
then
	echo '** built new '$TERMINFO
else
	echo '? tic could not build '$TERMINFO
	exit 1
fi

# Make a symbolic link to provide compatibility with applications that expect
# to find terminfo under /usr/lib.  That is, we'll _try_ to do that.  Not
# all systems support symbolic links, and those that do provide a variety
# of options for 'test'.
if test "$TICDIR" != "$TERMINFO" ; then
	( rm -f $TICDIR 2>/dev/null )
	if ( cd $TICDIR 2>/dev/null )
	then
		cd $TICDIR
		TICDIR=`pwd`
		if test $TICDIR != $TERMINFO ; then
			# Well, we tried.  Some systems lie to us, so the
			# installer will have to double-check.
			echo "Verify if $TICDIR and $TERMINFO are the same."
			echo "The new terminfo is in $TERMINFO; the other should be a link to it."
			echo "Otherwise, remove $TICDIR and link it to $TERMINFO."
		fi
	else
		cd $IP$prefix
		# Construct a symbolic link that only assumes $ticdir has the
		# same $prefix as the other installed directories.
		RELATIVE=`echo $ticdir|sed -e 's:^'$prefix'/::'`
		if test "$RELATIVE" != "$ticdir" ; then
			RELATIVE=../`echo $ticdir|sed -e 's:^'$prefix'/::' -e 's:^/::'`
		fi
		if ( ln -s $RELATIVE $TICDIR )
		then
			echo '** linked '$TICDIR' for compatibility'
		fi
	fi
fi
