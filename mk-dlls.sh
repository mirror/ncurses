#!/bin/sh
# $Id: mk-dlls.sh,v 1.2 2010/01/30 19:23:49 tom Exp $
##############################################################################
# Copyright (c) 2008,2010 Free Software Foundation, Inc.                     #
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
# Author: Juergen Pfeifer
#
# Build DLLs on MinGW
#
gcc -v 2>&1 | grep specs | grep mingw
if [ $? -eq 1 ]; then
  echo "$0 requires a mingw environment" >&2
else
  if [ -d lib ]; then
    cf="-shared"
    lf="--enable-auto-import"
    pushd lib 2>&1 >/dev/null
      for t in "" "t"
      do
        for m in "" "_g"
        do
          if [ -f libncurses${t}${m}.a ]; then
            f=libncurses${t}${m}.a
            g=`basename $f .a | cut -c 4-`
            gi=libw${g}.a
            td=tmp-${g}
            rm -rf "${td}"
            mkdir "${td}"
            cd "${td}"
              ar x ../${f} `ar t ../${f}`
              gcc $cf -o w${g}.dll -Wl,--out-implib,${gi} -Wl,--output-def,w${g}.def -Wl,$lf `ar t ../$f`
              lib //NOLOGO /MACHINE:i386 /DEF:w${g}.def
              rm -f `ar t ../$f`
              mv w${g}.dll ..
              mv w${g}.lib ..
              mv libw${g}.a ..
            cd ..
            rm -rf "${td}"

            for l in panel menu form
            do
              for f in lib${l}${t}${m}.a
              do
                g=`basename $f .a | cut -c 4-`
                td=tmp-${g}
                rm -rf "${td}"
                mkdir "${td}"
                echo $g
                cd "${td}"
                  ar x ../$f `ar t ../$f`
                  gcc $cf -o w${g}.dll -Wl,--out-implib,libw${g}.a -Wl,--output-def,w${g}.def -Wl,$lf `ar t ../$f` ../${gi}
                  lib //NOLOGO /MACHINE:i386 /DEF:w${g}.def
                  rm -f `ar t ../$f`
                  mv w${g}.dll ..
                  mv w${g}.lib ..
                  mv libw${g}.a ..
                cd ..
                rm -rf "${td}"
              done
            done
          fi
        done
      done
    popd
  else
    echo lib has not been built >&2
  fi
fi
