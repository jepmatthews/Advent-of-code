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
	elf1 := 0
	elf2 := 0
	elf3 := 0
	currentElf := 0
	for scanner.Scan() {
		if scanner.Text() == "" {
			if currentElf > elf1 {
				elf3 = elf2
				elf2 = elf1
				elf1 = currentElf
			} else if currentElf > elf2 {
				elf3 = elf2
				elf2 = currentElf
			} else if currentElf > elf3 {
				elf3 = currentElf
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
	if currentElf > elf1 {
		elf3 = elf2
		elf2 = elf1
		elf1 = currentElf
	} else if currentElf > elf2 {
		elf3 = elf2
		elf2 = currentElf
	} else if currentElf > elf3 {
		elf3 = currentElf
	}
	fmt.Println(currentElf)
	top3 := elf1 + elf2 + elf3
	fmt.Printf("1: %d,2: %d, 3: %d, Sum: %d\n", elf1, elf2, elf3, top3)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
