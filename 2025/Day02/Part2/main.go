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
			if isRepeated(val) {
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

func isRepeated(x int) bool {
	str := strconv.Itoa(x)
	for i := 1; i <= len(str)/2; i++ {
		if len(str)%i != 0 {
			continue
		}
		repeated := true
		for j := 0; j <= len(str)-(2*i); j += i {
			if str[j:j+i] != str[j+i:j+(2*i)] {
				repeated = false
				break
			}
		}
		if repeated == true {
			return true
		}
	}
	return false
}
