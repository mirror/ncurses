#!/bin/sh
##############################################################################
# Copyright (c) 2007 Free Software Foundation, Inc.                          #
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
# $Id: MKcaptab.sh,v 1.3 2007/07/28 22:14:57 tom Exp $
AWK=${1-awk}
OPT1=${2-0}
OPT2=${3-tinfo/MKcaptab.awk}
DATA=${4-../include/Caps}

cat <<'EOF'
/*
 *	comp_captab.c -- The names of the capabilities indexed via a hash
 *		         table for the compiler.
 *
 */

#include <ncurses_cfg.h>
#include <curses.priv.h>
#include <tic.h>
#include <term.h>

EOF

./make_hash 1 info <$DATA
./make_hash 3 cap  <$DATA

$AWK -f $OPT2 bigstrings=$OPT1 tablename=capalias <$DATA 

$AWK -f $OPT2 bigstrings=$OPT1 tablename=infoalias <$DATA

cat <<'EOF'

NCURSES_EXPORT(const struct name_table_entry *) _nc_get_table (bool termcap)
{
	return termcap ? _nc_cap_table: _nc_info_table ;
}

NCURSES_EXPORT(const short *) _nc_get_hash_table (bool termcap)
{
	return termcap ? _nc_cap_hash_table: _nc_info_hash_table ;
}

NCURSES_EXPORT(const struct alias *) _nc_get_alias_table (bool termcap)
{
	return termcap ? _nc_capalias_table: _nc_infoalias_table ;
}
EOF
