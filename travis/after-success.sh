#!/bin/bash

docker run --rm -v $package:/package -v $HOME:/home/ fhix/dr:latest R -e "devtools::build('/package/',path='/home'); styler::style_pkg('/package/')"

cd $HOME
PKG_TARBALL=$(ls -1t *.tar.gz | head -n 1);

docker run --rm -v $package:/package -v $HOME:/home/ -e PKG_TARBALL=$PKG_TARBALL -e GITHUB_PAT=$GITHUB_PAT fhix/dr:latest bash /package/travis/drat.sh

cd $package
bash travis/commit-source.sh
