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
			for distance > 0 {
				if position == 0 {
					position = 100
				}
				if distance >= position {
					distance -= position
					position = 0
					password += 1
				} else {
					position -= distance
					distance = 0
				}
			}

		} else {
			for distance > 0 {
				if distance+position >= 100 {
					distance -= 100 - position
					position = 0
					password += 1
				} else {
					position += distance
					distance = 0
				}
			}
		}

		fmt.Printf("Movement %s, new position: %d new password: %d\n", scanner.Text(), position, password)
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("The password is: %d", password)
}
