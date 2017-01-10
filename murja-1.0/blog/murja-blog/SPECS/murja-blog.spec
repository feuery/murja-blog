%define __jar_repack 0
Name: murja-blog
Version: 0.1.0
Release: SNAPSHOT20170110103032
Summary: Murja blogging system
License: (c) null
autoprov: no
autoreq: no
BuildArch: noarch
BuildRoot: /Volumes/Mac/murja-blog/blog/murja-blog/buildroot

%description

%install

if [ -d $RPM_BUILD_ROOT ];
then
  mv /Volumes/Mac/murja-blog/blog/murja-blog/tmp-buildroot/* $RPM_BUILD_ROOT
else
  mv /Volumes/Mac/murja-blog/blog/murja-blog/tmp-buildroot $RPM_BUILD_ROOT
fi

%files

%dir %attr(440,murja,murjagroup) "/usr/bin/murja"
