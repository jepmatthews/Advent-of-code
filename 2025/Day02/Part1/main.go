package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	f, err := os.Open("../input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	scanner.Scan()
	result := 0
	text := scanner.Text()

	for _, str := range strings.Split(text, ",") {
		split := strings.Split(str, "-")
		fmt.Printf("Start: %s, End: %s\n", split[0], split[1])
		rangeStart, err := strconv.Atoi(split[0])
		if err != nil {
			log.Fatal(err)
		}
		rangeEnd, err := strconv.Atoi(split[1])
		if err != nil {
			log.Fatal(err)
		}

		for val := rangeStart; val <= rangeEnd; val++ {
			if isPalindrome(val) {
				fmt.Printf("%d\n", val)
				result += val
			}
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("result: %d", result)
}

func isPalindrome(x int) bool {
	str := strconv.Itoa(x)
	if len(str)%2 != 0 {
		return false
	}
	if str[0:len(str)/2] == str[len(str)/2:] {
		return true
	}
	return false
}
