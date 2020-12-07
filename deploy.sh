# Requires: leiningen, java, git
# In case you want to deploy this software, edit these parameters to suit your needs.
# You might need to have a file at /etc/murja/config.edn for the build to pass
# Copy an example file from $project_root/config.demo.edn

build_dir=~/murja-deployment
github_url="https://github.com/feuery/murja-blog.git"
deploy_server="feuerx.net"
deploy_jar_path="/usr/bin/murja/murja.jar"
deploy_unitfile_path="/etc/systemd/system/murja.service"

echo This script pulls murja-blog from feuery\'s github-site, builds it and deploys on the machine you\'re running.
echo
echo Code will be pulled from $github_url to $build_dir.
echo This directory will be rm -rf\'d once on the startup and the second time after this script has ran.
echo
echo Final jar-file will be deployed to $deploy_server:$deploy_jar_path and the systemd unitfile to $deploy_server:$deploy_unitfile_path
echo

read -n1 -p "If this is okay, press return. Else press anything else." return_pressed

if [ ${#return_pressed} != 0 ]; then
    exit
fi

if [ -d $build_dir ]; then
    rm -rf $build_dir;
    echo $build_dir removed
fi

mkdir $build_dir
git clone $github_url $build_dir

if [ -d $build_dir ]; then

    echo
    echo Cloned from $github_url to $build_dir
    echo Commencing uberjarring

    cd $build_dir
    ./build.sh

    if [ -f $build_dir/blog/target/blog.jar ]; then
	echo target/blog.jar found! Uberjar succeeded!
	scp $build_dir/blog/target/blog.jar $deploy_server:$deploy_jar_path
	if [ $? -eq 0 ]; then
	    scp $build_dir/systemd/unitfile.service $deploy_server:$deploy_unitfile_path
	    if [ $? -eq 0 ]; then	
		echo
		echo $deploy_server:$deploy_jar_path and its systemd service file $deploy_server:$deploy_unitfile_path sent to server
	    else
		echo Sending unitfile failed
	    fi
	else
	    echo Sending jar-file failed
	fi
	
	echo Removing $build_dir
	rm -rf $build_dir
	echo Removed $build_dir
	echo
	echo
    else
	echo target/blog.jar not found! Uberjar didn\'t succeed!
    fi
else
    echo git clone $github_url $build_dir seems to have failed. Do you have git installed?
fi
