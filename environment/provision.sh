#!/bin/bash

if ! command -v kpcli &> /dev/null
then
    echo "Command kpcli not found. Please install it from your package manager"
    exit
fi

echo "This script provisions murja on the machine you're running this on"
echo "It builds the app, builds the murja docker image, and sets up the docker-compose.yml with secrets and kicks the docker containers running"

original_path=$(pwd)

./build.sh 

cd $original_path

if [[ -f ../murja/target/murja-2.0.0-SNAPSHOT-standalone.jar]];
then
    cp ../murja/target/murja-2.0.0-SNAPSHOT-standalone.jar ./murja.jar
    db_password=$(kpcli --kdb=./secrets.kdbx --command "show -f Root/db"|grep Pass:|cut -d: -f2|awk '{$1=$1};1')
    dns_api_key=$(kpcli --kdb=./secrets.kdbx --command "show -f Root/dns"|grep Pass:|cut -d: -f2|awk '{$1=$1};1')
    sed "s/{{db_password}}/$db_password/g" docker-compose.yml.template > docker-compose.yml
    sed "s/{{db_password}}/$db_password/g" config.edn.template > config.edn

    sed "s/{{dns}}/$dns_api_key/g" dns-conf/digitalocean.ini.template > dns-conf/digitalocean.ini

    if [[ -d www]] ;
    then
	rm -rf www
    fi

    git clone git@github.com:feuery/feuerx_frontpage.git www	

    docker-compose up
else
    echo "No murja jar found :("
fi
    
