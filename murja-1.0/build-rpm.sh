RPM_SOURCE_DIR=/usr/local/var/lib/rpmbuild/SOURCES
RPM_BUILD_ROOT=/usr/local/var/lib/rpmbuild/BUILD
source_file=https://github.com/feuery/murja-blog/archive/RPM-testitagi.tar.gz

curl -Lo $RPM_SOURCE_DIR/RPM-testitagi.tar.gz https://github.com/feuery/murja-blog/archive/RPM-testitagi.tar.gz
rpmbuild -ba ./specfile
