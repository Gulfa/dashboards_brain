#!/bin/bash

COMPUTER=$(cat /tmp/computer)

(
  flock -n 200 || exit 1

  source /etc/environment

  echo
  echo
  echo
  echo
  echo "****START****BRAIN****"

  /usr/local/bin/Rscript /r/brain/src/RunProcess.R

  echo "****END****BRAIN****"

) 200>/var/lock/.brain.exclusivelock
