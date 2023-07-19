#!/bin/bash

echo "compiling..."
raco make -v main.rkt || exit 1

echo "setting up..."
mv data.dat data.dat.bak
mv task.config task.config.bak

echo "running ..."
cp script/data.dat data.dat
time timeout 5 racket main.rkt < script/regression.in

mv data.dat.bak data.dat
mv task.config.bak task.config
