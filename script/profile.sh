#!/bin/bash

mv data.dat data.dat.bak
mv task.config task.config.bak

cp script/data.dat data.dat
racket -l errortrace -t script/profile.rkt

mv data.dat.bak data.dat
mv task.config.bak task.config
