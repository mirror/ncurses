dnl*****************************************************************************
dnl Copyright 1996,1997 by Thomas E. Dickey <dickey@clark.net>                 *
dnl All Rights Reserved.                                                       *
dnl                                                                            *
dnl Permission to use, copy, modify, and distribute this software and its      *
dnl documentation for any purpose and without fee is hereby granted, provided  *
dnl that the above copyright notice appear in all copies and that both that    *
dnl copyright notice and this permission notice appear in supporting           *
dnl documentation, and that the name of the above listed copyright holder(s)   *
dnl not be used in advertising or publicity pertaining to distribution of the  *
dnl software without specific, written prior permission. THE ABOVE LISTED      *
dnl COPYRIGHT HOLDER(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  *
dnl INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO     *
dnl EVENT SHALL THE ABOVE LISTED COPYRIGHT HOLDER(S) BE LIABLE FOR ANY         *
dnl SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER       *
dnl RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF       *
dnl CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN        *
dnl CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.                   *
dnl*****************************************************************************
dnl $Id: aclocal.m4,v 1.60 1997/05/10 15:56:16 tom Exp $
dnl Macros used in NCURSES auto-configuration script.
dnl
dnl ---------------------------------------------------------------------------
dnl Construct the list of include-options for the C programs in the Ada95
dnl binding.
AC_DEFUN([NC_ADA_INCLUDE_DIRS],
[
ACPPFLAGS="$ACPPFLAGS -I. -I../../include"
if test "$srcdir" != "."; then
	ACPPFLAGS="$ACPPFLAGS -I\$(srcdir)/../../include"
fi
if test -z "$GCC"; then
	ACPPFLAGS="$ACPPFLAGS -I\$(includedir)"
elif test "$includedir" != "/usr/include"; then
	if test "$includedir" = '${prefix}/include' ; then
		if test $prefix != /usr ; then
			ACPPFLAGS="$ACPPFLAGS -I\$(includedir)"
		fi
	else
		ACPPFLAGS="$ACPPFLAGS -I\$(includedir)"
	fi
fi
AC_SUBST(ACPPFLAGS)
])dnl
dnl ---------------------------------------------------------------------------
dnl Test if 'bool' is a builtin type in the configured C++ compiler.  Some
dnl older compilers (e.g., gcc 2.5.8) don't support 'bool' directly; gcc
dnl 2.6.3 does, in anticipation of the ANSI C++ standard.
dnl
dnl Treat the configuration-variable specially here, since we're directly
dnl substituting its value (i.e., 1/0).
AC_DEFUN([NC_BOOL_DECL],
[
AC_MSG_CHECKING([for builtin c++ bool type])
AC_CACHE_VAL(nc_cv_builtin_bool,[
	AC_TRY_COMPILE([],[bool x = false],
		[nc_cv_builtin_bool=1],
		[nc_cv_builtin_bool=0])
	])
if test $nc_cv_builtin_bool = 1
then	AC_MSG_RESULT(yes)
else	AC_MSG_RESULT(no)
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl Test for the size of 'bool' in the configured C++ compiler (e.g., a type).
dnl Don't bother looking for bool.h, since it's been deprecated.
AC_DEFUN([NC_BOOL_SIZE],
[
AC_MSG_CHECKING([for size of c++ bool])
AC_CACHE_VAL(nc_cv_type_of_bool,[
	rm -f nc_test.out
	AC_TRY_RUN([
#include <stdlib.h>
#include <stdio.h>
#if HAVE_BUILTIN_H
#include <builtin.h>
#endif
main()
{
	FILE *fp = fopen("nc_test.out", "w");
	if (fp != 0) {
		bool x = false;
		if (sizeof(x) == sizeof(int))       fputs("int",  fp);
		else if (sizeof(x) == sizeof(char)) fputs("char", fp);
		else if (sizeof(x) == sizeof(short))fputs("short",fp);
		else if (sizeof(x) == sizeof(long)) fputs("long", fp);
		fclose(fp);
	}
	exit(0);
}
		],
		[nc_cv_type_of_bool=`cat nc_test.out`],
		[nc_cv_type_of_bool=unknown],
		[nc_cv_type_of_bool=unknown])
	])
	rm -f nc_test.out
AC_MSG_RESULT($nc_cv_type_of_bool)
if test $nc_cv_type_of_bool = unknown ; then
	AC_MSG_WARN(Assuming unsigned for type of bool)
	nc_cv_type_of_bool=unsigned
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl Determine the default configuration into which we'll install ncurses.  This
dnl can be overridden by the user's command-line options.  There's two items to
dnl look for:
dnl	1. the prefix (e.g., /usr)
dnl	2. the header files (e.g., /usr/include/ncurses)
dnl We'll look for a previous installation of ncurses and use the same defaults.
dnl
dnl We don't use AC_PREFIX_DEFAULT, because it gets evaluated too soon, and
dnl we don't use AC_PREFIX_PROGRAM, because we cannot distinguish ncurses's
dnl programs from a vendor's.
AC_DEFUN([NC_CFG_DEFAULTS],
[
AC_MSG_CHECKING(for prefix)
if test "x$prefix" = "xNONE" ; then
	case "$nc_cv_systype" in
		# non-vendor systems don't have a conflict
	NetBSD|FreeBSD|Linux)	prefix=/usr
		;;
	*)	prefix=$ac_default_prefix
		;;
	esac
fi
AC_MSG_RESULT($prefix)
AC_MSG_CHECKING(for default include-directory)
test -n "$verbose" && echo 1>&6
for nc_symbol in \
	$includedir \
	$includedir/ncurses \
	$prefix/include \
	$prefix/include/ncurses \
	/usr/local/include \
	/usr/local/include/ncurses \
	/usr/include \
	/usr/include/ncurses
do
	nc_dir=`eval echo $nc_symbol`
	if test -f $nc_dir/curses.h ; then
	if ( fgrep NCURSES_VERSION $nc_dir/curses.h 2>&1 >/dev/null ) ; then
		includedir="$nc_symbol"
		test -n "$verbose"  && echo $ac_n "	found " 1>&6
		break
	fi
	fi
	test -n "$verbose"  && echo "	tested $nc_dir" 1>&6
done
AC_MSG_RESULT($includedir)
])dnl
dnl ---------------------------------------------------------------------------
dnl If we're trying to use g++, test if libg++ is installed (a rather common
dnl problem :-).  If we have the compiler but no library, we'll be able to
dnl configure, but won't be able to build the c++ demo program.
AC_DEFUN([NC_CXX_LIBRARY],
[
nc_cxx_library=unknown
if test $ac_cv_prog_gxx = yes; then
	AC_MSG_CHECKING([for libg++])
	nc_save="$LIBS"
	LIBS="$LIBS -lg++ -lm"
	AC_TRY_LINK([
#include <builtin.h>
	],
	[float foo=abs(1.0)],
	[nc_cxx_library=yes
	 CXXLIBS="$CXXLIBS -lg++ -lm"],
	[nc_cxx_library=no])
	LIBS="$nc_save"
	AC_MSG_RESULT($nc_cxx_library)
fi
])dnl
dnl ---------------------------------------------------------------------------
AC_DEFUN([NC_DIRS_TO_MAKE],
[
DIRS_TO_MAKE="lib"
for nc_item in $nc_list_models
do
	NC_OBJ_SUBDIR($nc_item,nc_subdir)
	DIRS_TO_MAKE="$DIRS_TO_MAKE $nc_subdir"
done
for nc_dir in $DIRS_TO_MAKE
do
	test ! -d $nc_dir && mkdir $nc_dir
done
AC_SUBST(DIRS_TO_MAKE)
])dnl
dnl ---------------------------------------------------------------------------
dnl
AC_DEFUN([NC_ERRNO],
[
AC_MSG_CHECKING([for errno external decl])
AC_CACHE_VAL(nc_cv_extern_errno,[
	AC_TRY_COMPILE([
#include <errno.h>],
		[int x = errno],
		[nc_cv_extern_errno=yes],
		[nc_cv_extern_errno=no])
	])
AC_MSG_RESULT($nc_cv_extern_errno)
test $nc_cv_extern_errno = yes && AC_DEFINE(HAVE_EXTERN_ERRNO)
])dnl
dnl ---------------------------------------------------------------------------
dnl Test for availability of useful gcc __attribute__ directives to quiet
dnl compiler warnings.  Though useful, not all are supported -- and contrary
dnl to documentation, unrecognized directives cause older compilers to barf.
AC_DEFUN([NC_GCC_ATTRIBUTES],
[cat > conftest.i <<EOF
#ifndef GCC_PRINTF
#define GCC_PRINTF 0
#endif
#ifndef GCC_SCANF
#define GCC_SCANF 0
#endif
#ifndef GCC_NORETURN
#define GCC_NORETURN /* nothing */
#endif
#ifndef GCC_UNUSED
#define GCC_UNUSED /* nothing */
#endif
EOF
if test -n "$GCC"
then
	AC_CHECKING([for gcc __attribute__ directives])
	changequote(,)dnl
cat > conftest.$ac_ext <<EOF
#line __oline__ "configure"
#include "confdefs.h"
#include "conftest.h"
#include "conftest.i"
#if	GCC_PRINTF
#define GCC_PRINTFLIKE(fmt,var) __attribute__((format(printf,fmt,var)))
#else
#define GCC_PRINTFLIKE(fmt,var) /*nothing*/
#endif
#if	GCC_SCANF
#define GCC_SCANFLIKE(fmt,var)  __attribute__((format(scanf,fmt,var)))
#else
#define GCC_SCANFLIKE(fmt,var)  /*nothing*/
#endif
extern void wow(char *,...) GCC_SCANFLIKE(1,2);
extern void oops(char *,...) GCC_PRINTFLIKE(1,2) GCC_NORETURN;
extern void foo(void) GCC_NORETURN;
int main(int argc GCC_UNUSED, char *argv[] GCC_UNUSED) { return 0; }
EOF
	changequote([,])dnl
	for nc_attribute in scanf printf unused noreturn
	do
		NC_UPPERCASE($nc_attribute,NC_ATTRIBUTE)
		nc_directive="__attribute__(($nc_attribute))"
		echo "checking for gcc $nc_directive" 1>&AC_FD_CC
		case $nc_attribute in
		scanf|printf)
		cat >conftest.h <<EOF
#define GCC_$NC_ATTRIBUTE 1
EOF
			;;
		*)
		cat >conftest.h <<EOF
#define GCC_$NC_ATTRIBUTE $nc_directive
EOF
			;;
		esac
		if AC_TRY_EVAL(ac_compile); then
			test -n "$verbose" && AC_MSG_RESULT(... $nc_attribute)
			cat conftest.h >>confdefs.h
#		else
#			sed -e 's/__attr.*/\/*nothing*\//' conftest.h >>confdefs.h
		fi
	done
else
	fgrep define conftest.i >>confdefs.h
fi
rm -rf conftest*

])dnl
dnl ---------------------------------------------------------------------------
dnl Check if the compiler supports useful warning options.  There's a few that
dnl we don't use, simply because they're too noisy:
dnl
dnl	-Wconversion (useful in older versions of gcc, but not in gcc 2.7.x)
dnl	-Wredundant-decls (system headers make this too noisy)
dnl	-Wtraditional (combines too many unrelated messages, only a few useful)
dnl
AC_DEFUN([NC_GCC_WARNINGS],
[nc_warn_CFLAGS=""
if test -n "$GCC"
then
	changequote(,)dnl
	cat > conftest.$ac_ext <<EOF
#line __oline__ "configure"
int main(int argc, char *argv[]) { return argv[argc-1] == 0; }
EOF
	changequote([,])dnl
	AC_CHECKING([for gcc warning options])
	nc_save_CFLAGS="$CFLAGS"
	nc_warn_CFLAGS="-W -Wall"
	nc_warn_CONST=""
	test "$with_ext_const" = yes && nc_warn_CONST="Wwrite-strings"
	for nc_opt in \
		Wbad-function-cast \
		Wcast-align \
		Wcast-qual \
		Winline \
		Wmissing-declarations \
		Wmissing-prototypes \
		Wnested-externs \
		Wpointer-arith \
		Wshadow \
		Wstrict-prototypes $nc_warn_CONST
	do
		CFLAGS="$nc_save_CFLAGS $nc_warn_CFLAGS -$nc_opt"
		if AC_TRY_EVAL(ac_compile); then
			test -n "$verbose" && AC_MSG_RESULT(... -$nc_opt)
			nc_warn_CFLAGS="$nc_warn_CFLAGS -$nc_opt"
		fi
	done
	rm -f conftest*
	CFLAGS="$nc_save_CFLAGS"
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl Verify Version of GNAT.
AC_DEFUN([NC_GNAT_VERSION],
[
changequote(<<, >>)dnl
nc_cv_gnat_version=`$nc_ada_make -v 2>&1 | grep '[0-9].[0-9][0-9]*' |\
  sed -e 's/[^0-9 \.]//g' | $AWK '{print $<<1>>;}'`
case $nc_cv_gnat_version in
  3.0[5-9]|3.[1-9]*|[4-9].*)
    ac_cv_prog_gnat_correct=yes
    ;;
  *) echo Unsupported GNAT version $nc_cv_gnat_version. Disabling Ada95 binding.
     ac_cv_prog_gnat_correct=no
     ;;
esac
changequote([, ])dnl
])
dnl ---------------------------------------------------------------------------
dnl Construct the list of include-options according to whether we're building
dnl in the source directory or using '--srcdir=DIR' option.  If we're building
dnl with gcc, don't append the includedir if it happens to be /usr/include,
dnl since that usually breaks gcc's shadow-includes.
AC_DEFUN([NC_INCLUDE_DIRS],
[
CPPFLAGS="$CPPFLAGS -I. -I../include"
if test "$srcdir" != "."; then
	CPPFLAGS="$CPPFLAGS -I\$(srcdir)/../include"
fi
if test -z "$GCC"; then
	CPPFLAGS="$CPPFLAGS -I\$(includedir)"
elif test "$includedir" != "/usr/include"; then
	if test "$includedir" = '${prefix}/include' ; then
		if test $prefix != /usr ; then
			CPPFLAGS="$CPPFLAGS -I\$(includedir)"
		fi
	else
		CPPFLAGS="$CPPFLAGS -I\$(includedir)"
	fi
fi
AC_SUBST(CPPFLAGS)
])dnl
dnl ---------------------------------------------------------------------------
dnl Append definitions and rules for the given models to the subdirectory
dnl Makefiles, and the recursion rule for the top-level Makefile.  If the
dnl subdirectory is a library-source directory, modify the LIBRARIES list in
dnl the corresponding makefile to list the models that we'll generate.
dnl
dnl For shared libraries, make a list of symbolic links to construct when
dnl generating each library.  The convention used for Linux is the simplest
dnl one:
dnl	lib<name>.so	->
dnl	lib<name>.so.<major>	->
dnl	lib<name>.so.<maj>.<minor>
AC_DEFUN([NC_LIB_RULES],
[
AC_REQUIRE([NC_SYSTYPE])
AC_REQUIRE([NC_VERSION])
for nc_dir in $SRC_SUBDIRS
do
	if test -f $srcdir/$nc_dir/modules; then

		nc_libs_to_make=
		for nc_item in $NC_LIST_MODELS
		do
			NC_LIB_SUFFIX($nc_item,nc_suffix)
			nc_libs_to_make="$nc_libs_to_make ../lib/lib${nc_dir}${nc_suffix}"
		done

		sed -e "s@\@LIBS_TO_MAKE\@@$nc_libs_to_make@" \
			$nc_dir/Makefile >$nc_dir/Makefile.out
		mv $nc_dir/Makefile.out $nc_dir/Makefile

		$AWK -f $srcdir/mk-0th.awk \
			name=$nc_dir \
			$srcdir/$nc_dir/modules >>$nc_dir/Makefile

		for nc_item in $NC_LIST_MODELS
		do
			echo 'Appending rules for '$nc_item' model ('$nc_dir')'
			NC_UPPERCASE($nc_item,NC_ITEM)
			NC_LIB_SUFFIX($nc_item,nc_suffix)
			NC_OBJ_SUBDIR($nc_item,nc_subdir)

			# These dependencies really are for development, not
			# builds, but they are useful in porting, too.
			nc_depend="../include/ncurses_cfg.h"
			if test "$srcdir" = "."; then
				nc_reldir="."
			else
				nc_reldir="\$(srcdir)"
			fi
			if test -f $srcdir/$nc_dir/$nc_dir.priv.h; then
				nc_depend="$nc_depend $nc_reldir/$nc_dir.priv.h"
			elif test -f $srcdir/$nc_dir/curses.priv.h; then
				nc_depend="$nc_depend $nc_reldir/curses.priv.h"
			fi
			$AWK -f $srcdir/mk-1st.awk \
				name=$nc_dir \
				MODEL=$NC_ITEM \
				model=$nc_subdir \
				suffix=$nc_suffix \
				DoLinks=$nc_cv_do_symlinks \
				rmSoLocs=$nc_cv_rm_so_locs \
				overwrite=$WITH_OVERWRITE \
				depend="$nc_depend" \
				$srcdir/$nc_dir/modules >>$nc_dir/Makefile
			test $nc_dir = ncurses && WITH_OVERWRITE=no
			$AWK -f $srcdir/mk-2nd.awk \
				name=$nc_dir \
				MODEL=$NC_ITEM \
				model=$nc_subdir \
				srcdir=$srcdir \
				echo=$WITH_ECHO \
				$srcdir/$nc_dir/modules >>$nc_dir/Makefile
		done
	fi

	echo '	cd '$nc_dir'; $(MAKE) $(NC_MFLAGS) [$]@' >>Makefile
done

for nc_dir in $SRC_SUBDIRS
do
	if test -f $srcdir/$nc_dir/modules; then
		echo >> Makefile
		if test -f $srcdir/$nc_dir/headers; then
cat >> Makefile <<NC_EOF
install.includes \\
NC_EOF
		fi
if test "$nc_dir" != "c++" ; then
echo 'lint \' >> Makefile
fi
cat >> Makefile <<NC_EOF
lintlib \\
install.libs \\
install.$nc_dir ::
	cd $nc_dir; \$(MAKE) \$(NC_MFLAGS) \[$]@
NC_EOF
	elif test -f $srcdir/$nc_dir/headers; then
cat >> Makefile <<NC_EOF

install.libs \\
install.includes ::
	cd $nc_dir; \$(MAKE) \$(NC_MFLAGS) \[$]@
NC_EOF
fi
done

cat >> Makefile <<NC_EOF

install.data ::
	cd misc; \$(MAKE) \$(NC_MFLAGS) \[$]@

install.man ::
	cd man; \$(MAKE) \$(NC_MFLAGS) \[$]@

distclean ::
	rm -f config.cache config.log config.status Makefile include/ncurses_cfg.h
	rm -f headers.sh headers.sed
	rm -rf \$(DIRS_TO_MAKE)
NC_EOF

dnl If we're installing into a subdirectory of /usr/include, etc., we should
dnl prepend the subdirectory's name to the "#include" paths.  It won't hurt
dnl anything, and will make it more standardized.  It's awkward to decide this
dnl at configuration because of quoting, so we'll simply make all headers
dnl installed via a script that can do the right thing.

rm -f headers.sed headers.sh

dnl ( generating this script makes the makefiles a little tidier :-)
echo creating headers.sh
cat >headers.sh <<NC_EOF
#!/bin/sh
# This shell script is generated by the 'configure' script.  It is invoked in a
# subdirectory of the build tree.  It generates a sed-script in the parent
# directory that is used to adjust includes for header files that reside in a
# subdirectory of /usr/include, etc.
PRG=""
while test \[$]# != 3
do
PRG="\$PRG \[$]1"; shift
done
DST=\[$]1
REF=\[$]2
SRC=\[$]3
echo installing \$SRC in \$DST
case \$DST in
/*/include/*)
	TMP=\${TMPDIR-/tmp}/\`basename \$SRC\`
	if test ! -f ../headers.sed ; then
		END=\`basename \$DST\`
		for i in \`cat \$REF/../*/headers |fgrep -v "#"\`
		do
			NAME=\`basename \$i\`
			echo "s/<\$NAME>/<\$END\/\$NAME>/" >> ../headers.sed
		done
	fi
	rm -f \$TMP
	sed -f ../headers.sed \$SRC > \$TMP
	eval \$PRG \$TMP \$DST
	rm -f \$TMP
	;;
*)
	eval \$PRG \$SRC \$DST
	;;
esac
NC_EOF

chmod 0755 headers.sh

for nc_dir in $SRC_SUBDIRS
do
	if test -f $srcdir/$nc_dir/headers; then
	cat >>$nc_dir/Makefile <<NC_EOF
\$(INSTALL_PREFIX)\$(includedir) :
	\$(srcdir)/../mkinstalldirs \[$]@

install \\
install.libs \\
install.includes :: \$(INSTALL_PREFIX)\$(includedir) \\
NC_EOF
		j=""
		for i in `cat $srcdir/$nc_dir/headers |fgrep -v "#"`
		do
			test -n "$j" && echo "		$j \\" >>$nc_dir/Makefile
			j=$i
		done
		echo "		$j" >>$nc_dir/Makefile
		for i in `cat $srcdir/$nc_dir/headers |fgrep -v "#"`
		do
			echo "	@ ../headers.sh \$(INSTALL_DATA) \$(INSTALL_PREFIX)\$(includedir) \$(srcdir) $i" >>$nc_dir/Makefile
		done
	fi
done

])dnl
dnl ---------------------------------------------------------------------------
dnl Compute the library-suffix from the given model name
AC_DEFUN([NC_LIB_SUFFIX],
[
	AC_REQUIRE([NC_SYSTYPE])
	AC_REQUIRE([NC_VERSION])
	case $1 in
	normal)  $2='.a'   ;;
	debug)   $2='_g.a' ;;
	profile) $2='_p.a' ;;
	shared)
		case $nc_cv_systype in
		NetBSD|FreeBSD)
			$2='.so.$(ABI_VERSION)' ;;
		HP_UX)	$2='.sl'  ;;
		*)	$2='.so'  ;;
		esac
	esac
])dnl
dnl ---------------------------------------------------------------------------
dnl Compute the string to append to -library from the given model name
AC_DEFUN([NC_LIB_TYPE],
[
	case $1 in
	normal)  $2=''   ;;
	debug)   $2='_g' ;;
	profile) $2='_p' ;;
	shared)  $2=''   ;;
	esac
])dnl
dnl ---------------------------------------------------------------------------
dnl Some systems have a non-ANSI linker that doesn't pull in modules that have
dnl only data (i.e., no functions), for example NeXT.  On those systems we'll
dnl have to provide wrappers for global tables to ensure they're linked
dnl properly.
AC_DEFUN([NC_LINK_DATAONLY],
[
AC_MSG_CHECKING([if data-only library module links])
AC_CACHE_VAL(nc_cv_link_dataonly,[
	rm -f conftest.a
	changequote(,)dnl
	cat >conftest.$ac_ext <<EOF
#line __oline__ "configure"
int	testdata[3] = { 123, 456, 789 };
EOF
	changequote([,])dnl
	if AC_TRY_EVAL(ac_compile) ; then
		mv conftest.o data.o && \
		( $AR $AR_OPTS conftest.a data.o ) 2>&5 1>/dev/null
	fi
	rm -f conftest.$ac_ext data.o
	changequote(,)dnl
	cat >conftest.$ac_ext <<EOF
#line __oline__ "configure"
int	testfunc()
{
#if defined(NeXT)
	exit(1);	/* I'm told this linker is broken */
#else
	extern int testdata[3];
	return testdata[0] == 123
	   &&  testdata[1] == 456
	   &&  testdata[2] == 789;
#endif
}
EOF
	changequote([,])dnl
	if AC_TRY_EVAL(ac_compile); then
		mv conftest.o func.o && \
		( $AR $AR_OPTS conftest.a func.o ) 2>&5 1>/dev/null
	fi
	rm -f conftest.$ac_ext func.o
	( eval $ac_cv_prog_RANLIB conftest.a ) 2>&5 >/dev/null
	nc_saveLIBS="$LIBS"
	LIBS="conftest.a $LIBS"
	AC_TRY_RUN([
	int main()
	{
		extern int testfunc();
		exit (!testfunc());
	}
	],
	[nc_cv_link_dataonly=yes],
	[nc_cv_link_dataonly=no],
	[nc_cv_link_dataonly=unknown])
	LIBS="$nc_saveLIBS"
	])
AC_MSG_RESULT($nc_cv_link_dataonly)
test $nc_cv_link_dataonly = no && AC_DEFINE(BROKEN_LINKER)
])dnl
dnl ---------------------------------------------------------------------------
dnl Some 'make' programs support $(MAKEFLAGS), some $(MFLAGS), to pass 'make'
dnl options to lower-levels.  It's very useful for "make -n" -- if we have it.
dnl (GNU 'make' does both :-)
AC_DEFUN([NC_MAKEFLAGS],
[
AC_MSG_CHECKING([for makeflags variable])
AC_CACHE_VAL(nc_cv_makeflags,[
	nc_cv_makeflags=''
	for nc_option in '$(MFLAGS)' '-$(MAKEFLAGS)'
	do
		cat >ncurses.tmp <<NC_EOF
all :
	echo '.$nc_option'
NC_EOF
		set nc_result=`${MAKE-make} -f ncurses.tmp 2>/dev/null`
		if test "$nc_result" != "."
		then
			nc_cv_makeflags=$nc_option
			break
		fi
	done
	rm -f ncurses.tmp
	])
AC_MSG_RESULT($nc_cv_makeflags)
AC_SUBST(nc_cv_makeflags)
])dnl
dnl ---------------------------------------------------------------------------
dnl Try to determine if the man-pages on the system are compressed, and if
dnl so, what format is used.  Use this information to construct a script that
dnl will install man-pages.
AC_DEFUN([NC_MAN_PAGES],
[AC_MSG_CHECKING(format of man-pages)
  if test -z "$MANPATH" ; then
    MANPATH="/usr/man:/usr/share/man"
  fi
  # look for the 'date' man-page (it's most likely to be installed!)
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  nc_form=unknown
  for nc_dir in $MANPATH; do
    test -z "$nc_dir" && nc_dir=/usr/man
    nc_rename=""
    nc_format=no
changequote({{,}})dnl
    for nc_name in $nc_dir/*/date.[01]* $nc_dir/*/date
changequote([,])dnl
    do
       nc_test=`echo $nc_name | sed -e 's/*//'`
       if test "x$nc_test" = "x$nc_name" ; then
	  case "$nc_name" in
	  *.gz) nc_form=gzip;     nc_name=`basename $nc_name .gz`;;
	  *.Z)  nc_form=compress; nc_name=`basename $nc_name .Z`;;
	  *.0)	nc_form=BSDI; nc_format=yes;;
	  *)    nc_form=cat;;
	  esac
	  break
       fi
    done
    if test "$nc_form" != "unknown" ; then
       break
    fi
  done
  IFS="$ac_save_ifs"
  if test "$prefix" = "NONE" ; then
     nc_prefix="$ac_default_prefix"
  else
     nc_prefix="$prefix"
  fi

  # Debian 'man' program?
  test -f /etc/debian_version && \
  nc_rename=`cd $srcdir;pwd`/man/man_db.renames

  test ! -d man && mkdir man

  # Construct a sed-script to perform renaming within man-pages
  if test -n "$nc_rename" ; then
    fgrep -v \# $nc_rename | \
    sed -e 's/^/s\//' \
        -e 's/\./\\./' \
        -e 's/	/ /g' \
        -e 's/[ ]\+/\//' \
        -e s/\$/\\\/g/ >man/edit_man.sed
  fi
  if test $nc_format = yes ; then
    nc_subdir='$mandir/cat'
  else
    nc_subdir='$mandir/man'
  fi

cat >man/edit_man.sh <<NC_EOF
changequote({{,}})dnl
#!/bin/sh
# this script is generated by the configure-script
prefix="$nc_prefix"
datadir="$datadir"
MKDIRS="`cd $srcdir;pwd`/mkinstalldirs"
INSTALL="$INSTALL"
INSTALL_DATA="$INSTALL_DATA"
TMP=\${TMPDIR-/tmp}/man\$\$
trap "rm -f \$TMP" 0 1 2 5 15

mandir=\{{$}}1
shift

for i in \{{$}}*
do
case \$i in
*.[0-9]*)
	section=\`expr "\$i" : '.*\\.\\([0-9]\\)[xm]*'\`;
	if [ ! -d $nc_subdir\${section} ]; then
		\$MKDIRS $nc_subdir\$section
	fi
	source=\`basename \$i\`
NC_EOF
if test -z "$nc_rename" ; then
cat >>man/edit_man.sh <<NC_EOF
	target=$nc_subdir\${section}/\$source
	sed -e "s,@DATADIR@,\$datadir," < \$i >\$TMP
NC_EOF
else
cat >>man/edit_man.sh <<NC_EOF
	target=\`grep "^\$source" $nc_rename | $AWK '{print \{{$}}2}'\`
	if test -z "\$target" ; then
		echo '? missing rename for '\$source
		target="\$source"
	fi
	target="$nc_subdir\$section/\$target"
	sed -e 's,@DATADIR@,\$datadir,' < \$i | sed -f edit_man.sed >\$TMP
NC_EOF
fi
if test $nc_format = yes ; then
cat >>man/edit_man.sh <<NC_EOF
	nroff -man \$TMP >\$TMP.out
	mv \$TMP.out \$TMP
NC_EOF
fi
case "$nc_form" in
compress)
cat >>man/edit_man.sh <<NC_EOF
	if ( compress -f \$TMP )
	then
		mv \$TMP.Z \$TMP
		target="\$target.Z"
	fi
NC_EOF
  ;;
gzip)
cat >>man/edit_man.sh <<NC_EOF
	if ( gzip -f \$TMP )
	then
		mv \$TMP.gz \$TMP
		target="\$target.gz"
	fi
NC_EOF
  ;;
BSDI)
cat >>man/edit_man.sh <<NC_EOF
	# BSDI installs only .0 suffixes in the cat directories
	target="\`echo \$target|sed -e 's/\.[1-9]\+.\?/.0/'\`"
NC_EOF
  ;;
esac
cat >>man/edit_man.sh <<NC_EOF
	echo installing \$target
	\$INSTALL_DATA \$TMP \$target
	;;
esac
done 
NC_EOF
changequote([,])dnl
chmod 755 man/edit_man.sh
AC_MSG_RESULT($nc_form)
])dnl
dnl ---------------------------------------------------------------------------
dnl Compute the object-directory name from the given model name
AC_DEFUN([NC_OBJ_SUBDIR],
[
	case $1 in
	normal)  $2='objects' ;;
	debug)   $2='obj_g' ;;
	profile) $2='obj_p' ;;
	shared)  $2='obj_s' ;;
	esac
])dnl
dnl ---------------------------------------------------------------------------
dnl Force $INSTALL to be an absolute-path.  Otherwise, edit_man.sh and the
dnl misc/tabset install won't work properly.  Usually this happens only when
dnl using the fallback mkinstalldirs script
AC_DEFUN([NC_PROG_INSTALL],
[AC_PROG_INSTALL
case $INSTALL in
/*)
  ;;
*)
changequote({{,}})dnl
  nc_dir=`echo $INSTALL|sed -e 's%/[^/]*$%%'`
  test -z "$nc_dir" && nc_dir=.
changequote([,])dnl
  INSTALL=`cd $nc_dir;pwd`/`echo $INSTALL | sed -e 's:^.*/::'`
  ;;
esac
])dnl
dnl ---------------------------------------------------------------------------
dnl Attempt to determine if we've got one of the flavors of regular-expression
dnl code that we can support.
AC_DEFUN([NC_REGEX],
[
AC_MSG_CHECKING([for regular-expression headers])
AC_CACHE_VAL(nc_cv_regex,[
AC_TRY_LINK([#include <sys/types.h>
#include <regex.h>],[
	regex_t *p;
	int x = regcomp(p, "", 0);
	int y = regexec(p, "", 0, 0, 0);
	regfree(p);
	],[nc_cv_regex="regex.h"],[
	AC_TRY_LINK([#include <regexp.h>],[
		char *p = compile("", "", "", 0);
		int x = step("", "");
	],[nc_cv_regex="regexp.h"],[
		AC_TRY_LINK([#include <regexpr.h>],[
			char *p = compile("", "", "");
			int x = step("", "");
		],[nc_cv_regex="regexpr.h"])])])
])
AC_MSG_RESULT($nc_cv_regex)
case $nc_cv_regex in
	regex.h)   AC_DEFINE(HAVE_REGEX_H) ;;
	regexp.h)  AC_DEFINE(HAVE_REGEXP_H) ;;
	regexpr.h) AC_DEFINE(HAVE_REGEXPR_H) ;;
esac
])dnl
dnl ---------------------------------------------------------------------------
dnl Attempt to determine the appropriate CC/LD options for creating a shared
dnl library.
dnl
dnl Note: $(LOCAL_LDFLAGS) is used to link executables that will run within the 
dnl build-tree, i.e., by making use of the libraries that are compiled in ../lib
dnl We avoid compiling-in a ../lib path for the shared library since that can
dnl lead to unexpected results at runtime.
dnl $(LOCAL_LDFLAGS2) has the same intention but assumes that the shared libraries
dnl are compiled in ../../lib
dnl
dnl The variable 'nc_cv_do_symlinks' is used to control whether we configure
dnl to install symbolic links to the rel/abi versions of shared libraries.
dnl
dnl Some loaders leave 'so_locations' lying around.  It's nice to clean up.
AC_DEFUN([NC_SHARED_OPTS],
[
	AC_REQUIRE([NC_SYSTYPE])
	AC_REQUIRE([NC_VERSION])
 	LOCAL_LDFLAGS=
 	LOCAL_LDFLAGS2=

	nc_cv_do_symlinks=no
	nc_cv_rm_so_locs=no

	case $nc_cv_systype in
	HP_UX)
		# (tested with gcc 2.7.2 -- I don't have c89)
		if test "${CC}" = "gcc"; then
			CC_SHARED_OPTS='-fPIC'
		else
			CC_SHARED_OPTS='+Z'
		fi
		MK_SHARED_LIB='$(LD) -b -o $[@]'
		;;
	IRIX*)
		# tested with IRIX 5.2 and 'cc'.
		if test "${CC}" = "gcc"; then
			CC_SHARED_OPTS='-fPIC'
		else
			CC_SHARED_OPTS='-KPIC'
		fi
		MK_SHARED_LIB='$(LD) -shared -rdata_shared -soname `basename $[@]` -o $[@]'
		nc_cv_rm_so_locs=yes
		;;
	Linux)
		# tested with Linux 1.2.8 and gcc 2.7.0 (ELF)
		CC_SHARED_OPTS='-fPIC'
 		MK_SHARED_LIB='gcc -o $[@].$(REL_VERSION) -shared -Wl,-soname,`basename $[@].$(ABI_VERSION)`,-stats'
		if test $DFT_LWR_MODEL = "shared" ; then
 			LOCAL_LDFLAGS='-Wl,-rpath,../lib'
 			LOCAL_LDFLAGS2='-Wl,-rpath,../../lib'
		fi
		nc_cv_do_symlinks=yes
		;;
	NetBSD|FreeBSD)
		CC_SHARED_OPTS='-fpic -DPIC'
		MK_SHARED_LIB='$(LD) -Bshareable -o $[@]'
		;;
	OSF1|MLS+)
		# tested with OSF/1 V3.2 and 'cc'
		# tested with OSF/1 V3.2 and gcc 2.6.3 (but the c++ demo didn't
		# link with shared libs).
		CC_SHARED_OPTS=''
 		MK_SHARED_LIB='$(LD) -o $[@].$(REL_VERSION) -shared -soname `basename $[@].$(ABI_VERSION)`'
		if test $DFT_LWR_MODEL = "shared" ; then
 			LOCAL_LDFLAGS='-Wl,-rpath,../lib'
 			LOCAL_LDFLAGS2='-Wl,-rpath,../../lib'
		fi
		nc_cv_do_symlinks=yes
		nc_cv_rm_so_locs=yes
		;;
	SunOS)
		# tested with SunOS 4.1.1 and gcc 2.7.0
		# tested with SunOS 5.3 (solaris 2.3) and gcc 2.7.0
		if test $ac_cv_prog_gcc = yes; then
			CC_SHARED_OPTS='-fpic'
		else
			CC_SHARED_OPTS='-KPIC'
		fi
		case `uname -r` in
		4.*)
			MK_SHARED_LIB='$(LD) -assert pure-text -o $[@].$(REL_VERSION)'
			;;
		5.*)
			MK_SHARED_LIB='$(LD) -d y -G -h `basename $[@].$(ABI_VERSION)` -o $[@].$(REL_VERSION)'
			;;
		esac
		nc_cv_do_symlinks=yes
		;;
	UNIX_SV)
		# tested with UnixWare 1.1.2
		CC_SHARED_OPTS='-KPIC'
		MK_SHARED_LIB='$(LD) -d y -G -o $[@]'
		;;
	*)
		CC_SHARED_OPTS='unknown'
		MK_SHARED_LIB='echo unknown'
		;;
	esac
	AC_SUBST(CC_SHARED_OPTS)
	AC_SUBST(MK_SHARED_LIB)
	AC_SUBST(LOCAL_LDFLAGS)
	AC_SUBST(LOCAL_LDFLAGS2)
])dnl
dnl ---------------------------------------------------------------------------
dnl Check for datatype 'speed_t', which is normally declared via either
dnl sys/types.h or termios.h
AC_DEFUN([NC_SPEED_TYPE],
[
AC_MSG_CHECKING([for speed_t])
AC_CACHE_VAL(nc_cv_type_speed_t,[
	AC_TRY_COMPILE([
#include <sys/types.h>
#if HAVE_TERMIOS_H
#include <termios.h>
#endif],
	[speed_t x = 0],
	[nc_cv_type_speed_t=yes],
	[nc_cv_type_speed_t=no])
	])
AC_MSG_RESULT($nc_cv_type_speed_t)
test $nc_cv_type_speed_t != yes && AC_DEFINE(speed_t,unsigned)
])dnl
dnl ---------------------------------------------------------------------------
dnl For each parameter, test if the source-directory exists, and if it contains
dnl a 'modules' file.  If so, add to the list $nc_cv_src_modules which we'll
dnl use in NC_LIB_RULES.
dnl
dnl This uses the configured value to make the lists SRC_SUBDIRS and
dnl SUB_MAKEFILES which are used in the makefile-generation scheme.
AC_DEFUN([NC_SRC_MODULES],
[
AC_MSG_CHECKING(for src modules)
TEST_DEPS="${LIB_PREFIX}${LIB_NAME}${DFT_DEP_SUFFIX}"
TEST_ARGS="-l${LIB_NAME}${DFT_ARG_SUFFIX}"
nc_cv_src_modules=
for nc_dir in $1
do
	if test -f $srcdir/$nc_dir/modules; then
		if test -z "$nc_cv_src_modules"; then
			nc_cv_src_modules=$nc_dir
		else
			nc_cv_src_modules="$nc_cv_src_modules $nc_dir"
		fi
		# Make the ncurses_cfg.h file record the library interface files as
		# well.  These are header files that are the same name as their
		# directory.  Ncurses is the only library that does not follow
		# that pattern.
		if test -f $srcdir/${nc_dir}/${nc_dir}.h; then
			NC_UPPERCASE($nc_dir,nc_have_include)
			AC_DEFINE_UNQUOTED(HAVE_${nc_have_include}_H)
			AC_DEFINE_UNQUOTED(HAVE_LIB${nc_have_include})
			TEST_DEPS="${LIB_PREFIX}${nc_dir}${DFT_DEP_SUFFIX} $TEST_DEPS"
			TEST_ARGS="-l${nc_dir}${DFT_ARG_SUFFIX} $TEST_ARGS"
		fi
	fi
done
AC_MSG_RESULT($nc_cv_src_modules)
TEST_ARGS="-L${LIB_DIR} $TEST_ARGS"
AC_SUBST(TEST_DEPS)
AC_SUBST(TEST_ARGS)

SRC_SUBDIRS="man include"
for nc_dir in $nc_cv_src_modules
do
	SRC_SUBDIRS="$SRC_SUBDIRS $nc_dir"
done
SRC_SUBDIRS="$SRC_SUBDIRS misc test"
test $nc_cxx_library != no && SRC_SUBDIRS="$SRC_SUBDIRS c++"

ADA_SUBDIRS=
if test "$ac_cv_prog_gnat_correct" = yes && test -d $srcdir/Ada95; then
   SRC_SUBDIRS="$SRC_SUBDIRS Ada95"
   ADA_SUBDIRS="gen ada_include samples"
fi

SUB_MAKEFILES=
for nc_dir in $SRC_SUBDIRS
do
	SUB_MAKEFILES="$SUB_MAKEFILES $nc_dir/Makefile"
done

if test -n "$ADA_SUBDIRS"; then
   for nc_dir in $ADA_SUBDIRS
   do	
      SUB_MAKEFILES="$SUB_MAKEFILES Ada95/$nc_dir/Makefile"
   done
   AC_SUBST(ADA_SUBDIRS)
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl	Remove "-g" option from the compiler options
AC_DEFUN([NC_STRIP_G_OPT],
[$1=`echo ${$1} | sed -e 's/-g //' -e 's/-g$//'`])dnl
dnl ---------------------------------------------------------------------------
dnl	Shorthand macro for substituting things that the user may override
dnl	with an environment variable.
dnl
dnl	$1 = long/descriptive name
dnl	$2 = environment variable
dnl	$3 = default value
AC_DEFUN([NC_SUBST],
[AC_CACHE_VAL(nc_cv_subst_$2,[
AC_MSG_CHECKING(for $1 (symbol $2))
test -z "[$]$2" && $2=$3
AC_MSG_RESULT([$]$2)
AC_SUBST($2)
nc_cv_subst_$2=[$]$2])
$2=${nc_cv_subst_$2}
])dnl
dnl ---------------------------------------------------------------------------
dnl	Check for declarion of sys_errlist in one of stdio.h and errno.h.  
dnl	Declaration of sys_errlist on BSD4.4 interferes with our declaration.
dnl	Reported by Keith Bostic.
AC_DEFUN([NC_SYS_ERRLIST],
[
AC_MSG_CHECKING([declaration of sys_errlist])
AC_CACHE_VAL(nc_cv_dcl_sys_errlist,[
	AC_TRY_COMPILE([
#include <stdio.h>
#include <sys/types.h>
#include <errno.h> ],
	[ char *c = (char *) *sys_errlist; ],
	[nc_cv_dcl_sys_errlist=yes],
	[nc_cv_dcl_sys_errlist=no])
	])
AC_MSG_RESULT($nc_cv_dcl_sys_errlist)
test $nc_cv_dcl_sys_errlist = yes && AC_DEFINE(HAVE_EXTERN_SYS_ERRLIST)
])dnl
dnl ---------------------------------------------------------------------------
dnl Derive the system-type (our main clue to the method of building shared
dnl libraries).
AC_DEFUN([NC_SYSTYPE],
[
AC_CACHE_VAL(nc_cv_systype,[
changequote(,)dnl
nc_cv_systype="`(uname -s || hostname || echo unknown) 2>/dev/null |sed -e s'/[:\/.-]/_/'g  | sed 1q`"
changequote([,])dnl
if test -z "$nc_cv_systype"; then nc_cv_systype=unknown;fi
])
AC_MSG_RESULT(System type is $nc_cv_systype)
])dnl
dnl ---------------------------------------------------------------------------
dnl On some systems ioctl(fd, TIOCGWINSZ, &size) will always return {0,0} until
dnl ioctl(fd, TIOCSWINSZ, &size) is called to explicitly set the size of the
dnl screen.
dnl
dnl Attempt to determine if we're on such a system by running a test-program.
dnl This won't work, of course, if the configure script is run in batch mode,
dnl since we've got to have access to the terminal.
dnl
dnl 1996/4/26 - Converted this into a simple test for able-to-compile, since
dnl we're reminded that _nc_get_screensize() does the same functional test.
AC_DEFUN([NC_TIOCGWINSZ],
[
AC_MSG_CHECKING([for working TIOCGWINSZ])
AC_CACHE_VAL(nc_cv_use_tiocgwinsz,[
	AC_TRY_RUN([
#if HAVE_TERMIOS_H
#include <termios.h>
#endif
#if SYSTEM_LOOKS_LIKE_SCO
/* they neglected to define struct winsize in termios.h -- it's only
   in termio.h	*/
#include	<sys/stream.h>
#include	<sys/ptem.h>
#endif
#if !defined(sun) || !defined(HAVE_TERMIOS_H)
#include <sys/ioctl.h>
#endif
int main()
{
	static	struct winsize size;
	int fd;
	for (fd = 0; fd <= 2; fd++) {	/* try in/out/err in case redirected */
		if (ioctl(0, TIOCGWINSZ, &size) == 0
		 && size.ws_row > 0
		 && size.ws_col > 0)
			exit(0);
	}
	exit(0);	/* in either case, it compiles & links ... */
}
		],
		[nc_cv_use_tiocgwinsz=yes],
		[nc_cv_use_tiocgwinsz=no],
		[nc_cv_use_tiocgwinsz=unknown])
	])
AC_MSG_RESULT($nc_cv_use_tiocgwinsz)
test $nc_cv_use_tiocgwinsz != yes && AC_DEFINE(BROKEN_TIOCGWINSZ)
])dnl
dnl ---------------------------------------------------------------------------
dnl
AC_DEFUN([NC_TYPE_SIGACTION],
[
AC_MSG_CHECKING([for type sigaction_t])
AC_CACHE_VAL(nc_cv_type_sigaction,[
	AC_TRY_COMPILE([
#include <signal.h>],
		[sigaction_t x],
		[nc_cv_type_sigaction=yes],
		[nc_cv_type_sigaction=no])
	])
AC_MSG_RESULT($nc_cv_type_sigaction)
test $nc_cv_type_sigaction = yes && AC_DEFINE(HAVE_TYPE_SIGACTION)
])
dnl ---------------------------------------------------------------------------
dnl Make an uppercase version of a given name
AC_DEFUN([NC_UPPERCASE],
[
changequote(,)dnl
$2=`echo $1 |tr '[a-z]' '[A-Z]'`
changequote([,])dnl
])dnl
dnl ---------------------------------------------------------------------------
dnl Get the version-number for use in shared-library naming, etc.
AC_DEFUN([NC_VERSION],
[
changequote(,)dnl
NCURSES_MAJOR="`egrep '^NCURSES_MAJOR[ 	]*=' $srcdir/dist.mk | sed -e 's/^[^0-9]*//'`"
NCURSES_MINOR="`egrep '^NCURSES_MINOR[ 	]*=' $srcdir/dist.mk | sed -e 's/^[^0-9]*//'`"
NCURSES_PATCH="`egrep '^NCURSES_PATCH[ 	]*=' $srcdir/dist.mk | sed -e 's/^[^0-9]*//'`"
changequote([,])dnl
nc_cv_abi_version=${NCURSES_MAJOR}
nc_cv_rel_version=${NCURSES_MAJOR}.${NCURSES_MINOR}
dnl Show the computed version, for logging
AC_MSG_RESULT(Configuring NCURSES $nc_cv_rel_version ABI $nc_cv_abi_version (`date`))
dnl We need these values in the generated headers
AC_SUBST(NCURSES_MAJOR)
AC_SUBST(NCURSES_MINOR)
AC_SUBST(NCURSES_PATCH)
dnl We need these values in the generated makefiles
AC_SUBST(nc_cv_rel_version)
AC_SUBST(nc_cv_abi_version)
AC_SUBST(nc_cv_builtin_bool)
AC_SUBST(nc_cv_type_of_bool)
])dnl
