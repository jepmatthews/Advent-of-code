#! /usr/bin/bash

if [ -z "$1" ]
then echo "Please provide the day"
exit
fi
day=$1
if [ ${#day} -eq 1 ]
then day="0$1"
fi
echo "Setting up day $day"
mkdir "./Day$day"
mkdir "./Day$day/Part1"
mkdir "./Day$day/Part2"
touch "./Day$day/Part1/sampleInput.txt"
touch "./Day$day/Part2/sampleInput.txt"
touch "./Day$day/input.txt"
cp "mainTemplate.go" "./Day$day/Part1/main.go"
cp "mainTemplate.go" "./Day$day/Part2/main.go"
cd "./Day$day/Part2"
go mod init "Advent-of-code/2022/Day01/Part2"
cd "../Part1"
go mod init "Advent-of-code/2022/Day01/Part1"