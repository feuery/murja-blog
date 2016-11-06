# -*- mode: ruby -*-

Vagrant.configure("2") do |config|
  config.vm.box = "ogarcia/archlinux-x64"

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  config.vm.network "public_network"
  config.vm.network "forwarded_port", guest: 5432, host: 5432
  config.vm.synced_folder ".", "/projektikansio"
  config.vm.provider :virtualbox do |v|
    v.name = 'db of blogthing'
    v.memory = 2048
    v.cpus = 2
  end

  config.vm.hostname = 'bloghost'

  # TODO: replace with a real, idempotent provision thing

  config.vm.provision "shell", inline: <<-SHELL
    set -e
    sudo pacman -Suy --noconfirm
    sudo pacman -S postgresql emacs-nox --noconfirm
    sudo systemctl enable postgresql
    if [ -f /var/lib/postgres/data/pg_hba.conf ]; then
      echo Db exists already
    else 
      echo Initing db
      su - postgres -c "initdb --locale en_US.UTF-8 -D '/var/lib/postgres/data'"
    fi
    
    echo "listen_addresses = '*'" >> /var/lib/postgres/data/postgresql.conf
    echo "host    all             all             10.0.2.2/32                trust" >> /var/lib/postgres/data/pg_hba.conf


    sudo systemctl start postgresql

    echo Creating db blogdb
    createdb -U postgres blogdb

    echo Creating db user blogiadmin
    su - postgres -c "createuser -d -l -r -s blogiadmin"

    echo Done
    exit
  SHELL
 
 # Enable provisioning with a shell script. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.
  # config.vm.provision "shell", inline: <<-SHELL
  #   apt-get update
  #   apt-get install -y apache2
  # SHELL
end
