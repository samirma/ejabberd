./configure --prefix=/opt/ejabberd --enable-pgsql
make 
make install 
cp external_scripts/auth.py /opt/ejabberd/scripts/auth.py
