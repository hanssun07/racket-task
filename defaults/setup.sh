#!/bin/bash

echo "compiling..."
raco make src/main.rkt

echo "setting up defaults..."
mkdir data
echo '()(("admin"))' > data/data.dat
{
    echo '(domain :'
    echo '    (datafile "data/data.dat")'
    echo '    (login "admin")'
} > data.config
mv defaults/run.sh .

cd data
git init

cd ..
rm -r defaults
echo "done"
