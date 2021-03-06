#!/bin/bash

inputLocation=compi2
toolsLocation=testing-tools
testLocation=do-not-touch

cp $toolsLocation/* $testLocation
cp $inputLocation/* $testLocation
cd $testLocation


echo "_____________________________________________________________________________________________________________________________"

# --HW1-------------------------------------------------------

echo HW1 compare tests:
#echo commented out...
scheme --script compare-tests-hw1.scm | grep TESTS






# --HW2-------------------------------------------------------

# .::part1::.
echo HW2 part1 compare tests:
scheme --script compare-tests-hw2.scm | grep TESTS

echo HW2 part1 my compare tests:
scheme --script my-compare-tests-hw2.scm | grep TESTS

# .::part2::.
echo HW2 part2 compare tests:
scheme --script compare-tests-hw2-CSE.scm | grep TESTS

echo HW2 part2 my compare tests:
scheme --script my-compare-tests-hw2-CSE.scm | grep TESTS














