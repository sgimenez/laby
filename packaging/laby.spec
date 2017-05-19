Name:           laby
Version:        0.6.4
Release:        3%{?dist}
Summary:        Learn programming, playing with ants and spider webs

License:        GPLv3+
URL:            https://sgimenez.github.io/laby/
Source0:        https://github.com/sgimenez/%{name}/archive/%{name}-%{version}.tar.gz
Patch1: 0001-Adapts-to-smaller-screens.patch
Patch2: 0002-Packaging-Metadata.patch
Patch3: 0003-Use-proper-Makefile-syntax.patch
Patch4: 0004-src-gfx.ml-Fix-height-of-help-sourceview.patch
Patch5: 0005-Tweaking-the-vertical-split-to-avoid-scroll-bars.patch
Patch7: 0007-Desktop-file-translations-provided-by-glixx.patch
Patch8: 0008-Translation-for-desktop-entry-GenericName.patch
Patch9: 0009-Fix-the-FSF-address.patch

# All above patches have been already merged in upstream master
# For genertating them:
# git clone https://github.com/sgimenez/laby.git
# git format-patch e6d783468b6d1273e6c7b40015367d81467c0205
# note patch 0006 has been skipped since it cause build failure on CentOS

BuildRequires:  ocaml >= 3.10.0
BuildRequires:  ocaml-findlib-devel
BuildRequires:  ocaml-lablgtk-devel >= 2.14.0
BuildRequires:  ocaml-ocamldoc
BuildRequires:  chrpath
BuildRequires:  gtksourceview2-devel >= 2.10
BuildRequires:  libappstream-glib
BuildRequires:  desktop-file-utils

%if 0%{?fedora} >= 26
BuildRequires:  ocaml-ocamlbuild
%endif

# Note: rpmlint suggest to add
# BuildRequires: python2-devel
# or
# BuildRequires: python3-devel
# but they're not used during the build so they've not been added.

Requires:  gtksourceview2 >= 2.10
Requires:  ocaml-lablgtk >= 2.14.0


%description
Laby is a small program to learn how to program with ants and spider webs.
You have to move an ant out of a labyrinth, avoid spider webs, move rocks, etc.


%prep
%setup -q -n %{name}-%{name}-%{version}
%autosetup -n %{name}-%{name}-%{version} -p1

%build
make native

%install
rm -rf %{buildroot}
export DESTDIR=%{buildroot}
make install

appstream-util validate-relax --nonet %{buildroot}/%{_datadir}/appdata/*.appdata.xml
desktop-file-validate %{buildroot}/%{_datadir}/applications/%{name}.desktop

%post
/bin/touch --no-create %{_datadir}/icons/hicolor &>/dev/null || :

%postun
if [ $1 -eq 0 ] ; then
    /bin/touch --no-create %{_datadir}/icons/hicolor &>/dev/null
    /usr/bin/gtk-update-icon-cache %{_datadir}/icons/hicolor &>/dev/null || :
fi

%posttrans
/usr/bin/gtk-update-icon-cache %{_datadir}/icons/hicolor &>/dev/null || :


%files
%license COPYRIGHT
%license gpl-3.0.txt
%doc AUTHORS
%{_bindir}/%{name}
%{_datadir}/%{name}/

# Note above contains also:
# /usr/share/laby/mods/c/lib/robot.h
# /usr/share/laby/mods/cpp/lib/robot.h
# Which rpmlint suggest to have in -devel subpackage.
# This is intentional. The game teach you also how to program in C and in order
# to move the ant, you'll need the robot.h header file. It isn't the use case
# addressed by -devel subpackages.

%{_datadir}/appdata/%{name}.appdata.xml
%{_datadir}/applications/%{name}.desktop
%{_datadir}/icons/hicolor/scalable/apps/%{name}.svg


%changelog
* Fri May 19 2017 Sandro Bonazzola <sandro.bonazzola@gmail.com> - 0.6.4-3
- Addressed comments #3-7 from rhbz#1450679

* Tue May 16 2017 Sandro Bonazzola <sandro.bonazzola@gmail.com> - 0.6.4-2
- Add Fedora >= 26 support

* Sun May 14 2017 Sandro Bonazzola <sandro.bonazzola@gmail.com> - 0.6.4-1
- Initial packaging
