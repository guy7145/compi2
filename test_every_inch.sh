#!/bin/bash

inputLocation=compi2
toolsLocation=testing-tools
testLocation=do-not-touch

cp $toolsLocation/* $testLocation
cp $inputLocation/* $testLocation
cd $testLocation

echo HW1:
echo commented out...
sleep 1

# scheme --script compare-tests-hw1.scm | grep TESTS
echo HW2:
scheme --script compare-tests-hw2.scm #| grep TESTS
scheme --script my-compare-tests-hw2.scm #| grep TESTS


