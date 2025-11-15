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
		line1 := scanner.Text()
		scanner.Scan()
		line2 := scanner.Text()
		scanner.Scan()
		line3 := scanner.Text()
		rucksack1Map := makeRucksackMap(line1)
		rucksack2Map := makeRucksackMap(line2)

		for _, char := range line3 {
			if _, found := rucksack1Map[char]; found {
				if _, found := rucksack2Map[char]; found {
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

	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Total Priority: %d", totalPriority)
}

func makeRucksackMap(backPackStr string) map[rune]bool {
	result := map[rune]bool{}
	for _, char := range backPackStr {
		result[char] = true
	}
	return result
}
