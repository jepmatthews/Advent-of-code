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
	points := 0
	for scanner.Scan() {
		switch scanner.Text() {
		case "A X": // Rock Loss(Scissors) - Loss (0) + Scissors (3)
			points += 3
		case "B X": // Paper Loss(Rock) - Loss (0) + Rock (1)
			points += 1
		case "C X": // Scissors(Paper) - Loss (0) + Paper (2)
			points += 2
		case "A Y": // Rock Draw(Rock) - Draw (3) + Rock (1)
			points += 4
		case "B Y": // Paper Draw(Paper) - Draw (3) + Paper (2)
			points += 5
		case "C Y": // Scissors Draw(Scissors) - Draw (3) + Scissors (3)
			points += 6
		case "A Z": // Rock Win(Paper) - Win (6) + Paper (2)
			points += 8
		case "B Z": // Paper Win(Scissors) - Win (6) + Scissors (3)
			points += 9
		case "C Z": // Scissors Win(Rock) - Win (6) + Rock (1)
			points += 7

		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Total score: %d\n", points)
}
