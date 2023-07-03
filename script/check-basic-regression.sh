#!/bin/bash

mv data.dat data.dat.bak
mv task.config task.config.bak

cp script/data.dat data.dat
cat script/regression.in | racket main.rkt

mv data.dat.bak data.dat
mv task.config.bak task.config
