# call 'SETUP_LOCAL_VM=1 vagrant up' when setting up a new dev vm
set -eu

if [[ "$(/usr/sbin/getenforce)" == "Enforcing" ]]; then
    echo "SELinux seems to be running. Stuff WILL fail"
    echo "Disable it by running 'sudo sed -i 's/enforcing/disabled/g'  /etc/selinux/config /etc/selinux/config' and restarting the vm"
fi   

sudo dnf install -y postgresql-server.x86_64 emacs-nox.x86_64 git nginx

sudo systemctl enable postgresql
sudo systemctl start nginx

SETUP_LOCAL_VM=1 		# TODO Don't cmmit
PG_HBA_FILE=/var/lib/pgsql/data/pg_hba.conf
PG_CONF_FILE=/var/lib/pgsql/data/postgresql.conf

PG_HBA_CONFIG="# TYPE  DATABASE        USER            ADDRESS                 METHOD                                                                                                                                                                       

# This config should allow connecting through a vagrant-forwarded port                                                                                                                                                                       

local   all             all                                     peer
host    all             all             127.0.0.1/32            trust
host    all             all             ::1/128                 trust"

if [ -f $PG_HBA_FILE ]; then
    echo Db exists already
else 
    echo Initing db
    sudo postgresql-setup --initdb --unit postgresql
fi

if [[ $SETUP_LOCAL_VM -eq 1 ]]; then
    echo "SETUP_LOCAL_VM defined, assuming you're setting up a dev vm"
    LISTEN_ALL="listen_addresses = '*'"    
    
    if sudo grep -q "$PG_HBA_CONFIG" $PG_HBA_FILE ; then
	echo "Setting up pg\'s listening config"
	sudo su -c "echo \"$PG_HBA_CONFIG\" > \"$PG_HBA_FILE\""
    else
	echo "We're listening everywhere. NOTE THAT THIS ISN'T SUPPOSED TO HAPPEN ON FEUERX.NET";
    fi

    if sudo grep -q "$LISTEN_ALL" $PG_CONF_FILE; then
	echo "Setting up listen_to_all. THIS ISN'T SUPPOSED TO HAPPEN ON FEUERX.NET"
	sudo su -c "echo \"$LISTEN_ALL\" >> \"$PG_CONF_FILE\""
    else
	echo "PGSQL is Listening to all. THIS ISN'T SUPPOSED TO HAPPEN ON FEUERX.NET"
    fi
else
    echo 'No SETUP_LOCAL_VM defined, are you setting up a production feuerx.net?'
    if sudo grep -q "$SEARCH_TERM" "$PG_HBA_FILE"; then
	echo "We're listening everywhere and SETUP_LOCAL_VM isn't SET. NOTE THAT THIS ISN'T SUPPOSED TO HAPPEN ON FEUERX.NET";
    fi
fi

if sudo grep -q "#log_destination = 'stderr'"; then
    echo 'PGSQL log is set up correctly'o
else
    sudo sed -i "s/#log_destination = 'stderr'/log_destination = 'syslog, stderr'/g" $PG_CONF_FILE $PG_CONF_FILE
fi

sudo systemctl start postgresql

if [[ $(sudo su postgres -c "cd ~; psql -tAc \"SELECT 1 FROM pg_roles WHERE rolname='blogiadmin'\"") -eq 1 ]]; then
    echo User blogiadmin exists
else
    echo Creating db user blogiadmin
    su - postgres -c "createuser -d -l -r -s blogiadmin"
fi

if sudo su postgres -c "cd ~; psql -lqt" | cut -d \| -f 1 | grep -qw blogidb; then
    echo Database blogidb exists
else 
    echo Creating db blogdb
    sudo su postgres -c "cd ~; psql -tAc \"CREATE DATABASE blogidb OWNER blogiadmin\""
fi

# echo Done
# exit

echo Lollero
