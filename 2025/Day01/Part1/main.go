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

	position := 50
	password := 0

	for scanner.Scan() {
		distance, err := strconv.Atoi(scanner.Text()[1:])
		if err != nil {
			log.Fatal(err)
		}
		if scanner.Text()[0] == 'L' {
			position -= distance
		} else {
			position += distance
		}

		position %= 100
		fmt.Printf("Movement %s, new position: %d\n", scanner.Text(), position)
		if position == 0 {
			password += 1
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("The password is: %d", password)
}
