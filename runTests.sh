#!/bin/bash

red='\033[0;31m'
green='\033[0;32m'
yellow='\033[0;33m'
nocolor='\033[0m'

successCount=0
failCount=0

mkdir -p bin/
cd src/
ghc -O2 main.hs -outputdir ../bin -o ../bin/haskell-alpha || exit 1
cd ..

function runTest
{
    local input=$(head -n 1 "$1")
    local expected=$(tail -n 1 "$1")

	tryingText="${yellow}TRYING${nocolor} test $1 formula '$input' expecting '$expected'"
    printf "$tryingText\n"
	local output=$(bin/haskell-alpha "$input")
	local status=$?
    local result=$(echo "$output" | tail -n 1)


    if [[ "$result" != "$expected" ]] || [[ $status -ne 0 ]]; then
		printf "${red}ERROR${nocolor} running test $1\n"
        echo "$output"
		failCount=$(($failCount + 1))
	else
        printf "\e[1A"
        printf "$tryingText" | sed 's/./ /g'
		printf "\r${green}SUCCESS${nocolor} running test $1\n"
        successCount=$(($successCount + 1))
	fi
}

for file in tests/*.test; do
    runTest "$file"
done

printf "$green$successCount tests passed$nocolor\n"
if [[ $failCount -ne 0 ]]; then
    printf "$red$failCount tests failed$nocolor\n"
    exit 1
fi
