#!/bin/bash

dir="~/task"
cd $dir

pull() {
    pushd . >/dev/null
    cd "$1"
    echo -n "$1: "
    git pull --rebase || exit 1
    popd >/dev/null
}
pull data/
wait

echo
racket main.rkt
echo

push() {
    pushd . >/dev/null
    cd "$1"
    echo -n "$1: "
    git add data.dat
    git diff --staged --quiet && echo "No changes to commit." || {
        git commit -m "[update]"
        git push
    }
    popd >/dev/null
}
push data/
