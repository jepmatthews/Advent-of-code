package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	f, err := os.Open("../input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	result := 0

	for scanner.Scan() {
		line := scanner.Text()
		firstDigit, err := strconv.Atoi(line[len(line)-2 : len(line)-1])
		if err != nil {
			log.Fatal(err)
		}
		secondDigit, err := strconv.Atoi(line[len(line)-1 : len(line)])
		if err != nil {
			log.Fatal(err)
		}
		for i := len(line) - 3; i >= 0; i-- {
			digit, err := strconv.Atoi(line[i : i+1])
			if err != nil {
				log.Fatal(err)
			}
			if digit >= firstDigit {
				if firstDigit > secondDigit {
					secondDigit = firstDigit
				}
				firstDigit = digit
			}
		}

		maxJolt := (10 * firstDigit) + secondDigit
		fmt.Println(maxJolt)
		result += maxJolt
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Result: %d\n", result)
}
