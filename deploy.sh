# Probably not going to work if directly run from a shell

sudo mkdir /usr/bin/murja
sudo cp blog/target/blog.jar /usr/bin/murja/murja.jar;
sudo cp systemd/unitfile.service /etc/systemd/system/murja.service;
