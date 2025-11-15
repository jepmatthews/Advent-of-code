package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	fmt.Println("Hello, World!")
	f, err := os.Open("./input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	maxElf := 0
	currentElf := 0
	for scanner.Scan() {
		if scanner.Text() == "" {
			if currentElf > maxElf {
				maxElf = currentElf
			}
			fmt.Println(currentElf)
			currentElf = 0
		} else {
			val, err := strconv.Atoi(scanner.Text())
			if err != nil {
				log.Fatal(err)
			}
			currentElf += val
		}

	}
	if currentElf > maxElf {
		maxElf = currentElf
	}
	fmt.Println(currentElf)
	fmt.Printf("Max: %d\n", maxElf)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
