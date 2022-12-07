#!/usr/bin/env bash

for day in $(seq -f '%02g' 1 25); do
  filename="Day${day}.hs"
  sed -e "s/_DAY_/${day}/g" script/DayTemplate.hs > src/$filename
done
