#!/bin/bash

set -e

day=$(printf "%02d" "$1")

mkdir -p "src/Day${day}"

sed "s/NN/${day}/g" templates/src/DayNN.elm > "src/Day${day}.elm"
sed "s/NN/${day}/g" templates/src/DayNN/Input.elm > "src/Day${day}/Input.elm"
sed "s/NN/${day}/g" templates/tests/DayNNTest.elm > "tests/Day${day}Test.elm"
