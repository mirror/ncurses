Summary: AdaCurses - Ada95 binding for ncurses
%define AppProgram AdaCurses
%define AppVersion MAJOR.MINOR
%define AppRelease YYYYMMDD
# $Id: AdaCurses.spec,v 1.5 2011/03/26 20:41:44 tom Exp $
Name: %{AppProgram}
Version: %{AppVersion}
Release: %{AppRelease}
License: MIT
Group: Applications/Development
URL: ftp://invisible-island.net/%{AppProgram}
Source0: %{AppProgram}-%{AppRelease}.tgz
Packager: Thomas Dickey <dickey@invisible-island.net>

%description
This is the Ada95 binding from the ncurses MAJOR.MINOR distribution, for
patch-date YYYYMMDD.

In addition to a library, this package installs sample programs in
"bin/AdaCurses" to avoid conflict with other packages.
%prep

%setup -q -n %{AppProgram}-%{AppRelease}

%build

INSTALL_PROGRAM='${INSTALL}' \
	./configure \
		--target %{_target_platform} \
		--prefix=%{_prefix} \
		--bindir=%{_bindir} \
		--libdir=%{_libdir} \
		--mandir=%{_mandir} \
		--datadir=%{_datadir} \
		--with-ada-sharedlib

make

%install
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

make install               DESTDIR=$RPM_BUILD_ROOT

( cd samples &&
  make install.examples \
	BINDIR=$RPM_BUILD_ROOT%{_bindir}/%{AppProgram}
)

%clean
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%{_bindir}/adacurses*-config
%{_bindir}/%{AppProgram}/*
%{_libdir}/libAdaCurses.*
%{_libdir}/ada/adalib/libAdaCurses.*
%{_libdir}/ada/adalib/terminal_interface*
%{_mandir}/man1/adacurses*-config.1*
%{_datadir}/ada/adainclude/terminal_interface*

%changelog
# each patch should add its ChangeLog entries here

* Fri Mar 25 2010 Thomas Dickey
- initial version
