set -e
sudo pacman -Syy
sudo pacman -S postgresql emacs-nox nginx git --noconfirm
sudo pacman -Suy --noconfirm

sudo systemctl enable postgresql nginx
if [ -f /var/lib/postgres/data/pg_hba.conf ]; then
    echo Db exists already
else 
    echo Initing db
    su - postgres -c "initdb --locale en_US.UTF-8 -D '/var/lib/postgres/data'"
fi

LISTEN_ALL="listen_addresses = '*'"
PG_CONF_FILE=/var/lib/postgres/data/postgresql.conf
if sudo grep -q "$LISTEN_ALL" $PG_CONF_FILE; then
    echo Already listening
else
    echo "$LISTEN_ALL"  > $PG_CONF_FILE
    echo "Listening set up"
fi

HOST_ALL="host    all             all             10.0.2.2/32                trust"
HBA_FILE=/var/lib/postgres/data/pg_hba.conf

if sudo grep -q "$HOST_ALL" $HBA_FILE; then
    echo HBA file configured
else
    echo Configuring HBA file
    echo "$HOST_ALL" >> $HBA_FILE
fi

sudo systemctl start postgresql

# Setting up postgresql
if sudo su postgres -c "cd ~; psql -lqt" | cut -d \| -f 1 | grep -qw blogdb; then
    echo Database blogdb exists
else 
    echo Creating db blogdb
    createdb -U postgres blogdb
fi

if [[ $(sudo su postgres -c "cd ~; psql -tAc \"SELECT 1 FROM pg_roles WHERE rolname='blogiadmin'\"") -eq 1 ]]; then
    echo User blogiadmin exists
else
    echo Creating db user blogiadmin
    su - postgres -c "createuser -d -l -r -s blogiadmin"
fi

# Setting up nginx
WWW_ROOT=/usr/share/nginx/html

cp /projektikansio/nginx.conf  /etc/nginx/nginx.conf

# Reinstalling nginx craps our $WWW_ROOT
rm -rf $WWW_ROOT
git clone https://github.com/feuery/feuerx_frontpage.git $WWW_ROOT

sudo systemctl restart nginx

echo Done
exit
