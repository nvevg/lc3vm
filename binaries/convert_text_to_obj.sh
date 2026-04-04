#!/usr/bin/bash

tr ' \n' '\n' < test_add.hex | sed '/^$/d' | while read w; do
  printf "\\x${w%??}\\x${w#??}"
done > test_add.obj
