#!/usr/bin/bash

tr ' \n' '\n' < test_kbsr_access.hex | sed '/^$/d' | while read w; do
  printf "\\x${w%??}\\x${w#??}"
done > test_kbsr_access.obj
