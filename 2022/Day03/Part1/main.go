package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	f, err := os.Open("../input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	totalPriority := 0

	for scanner.Scan() {
		midPoint := len(scanner.Text()) / 2
		firstCompartment := map[rune]bool{}
		for i, char := range scanner.Text() {
			if i < midPoint {
				firstCompartment[char] = true
			} else if _, found := firstCompartment[char]; found {
				priority := int(char) - 38
				if priority >= 59 {
					priority -= 58
				}
				fmt.Printf("Priority: %c %d\n", char, priority)
				totalPriority += priority
				break
			}
		}

	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Total Priority: %d", totalPriority)
}
