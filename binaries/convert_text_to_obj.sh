#!/usr/bin/bash

tr ' \n' '\n' < test_br.hex | sed '/^$/d' | while read w; do
  printf "\\x${w%??}\\x${w#??}"
done > test_br.obj
