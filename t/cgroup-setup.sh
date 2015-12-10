#!/bin/bash

name=${1:-$(whoami)}

echo "Checking cgroup cpu,cpuacct,memory:/$name"

if cgexec -g cpu,cpuacct,memory:/$name true
then
    echo "cgroup already exists, exiting."
    exit
else
    echo "Creating cgroup cpu,cpuacct,memory:/$name"
    SUDO_ASKPASS=/usr/bin/ssh-askpass sudo -A cgcreate -a $name:$name -t $name:$name -g cpu,cpuacct,memory:/$name
fi
