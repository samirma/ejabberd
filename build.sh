./configure --prefix=/opt/ejabberd --enable-pgsql
make 
make install 
cp external_scripts/* /opt/ejabberd/scripts/
