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
		digits := make([]int, 12)
		lineSlice := make([]int, len(line))
		for i := 0; i < len(line); i++ {
			lineSlice[i], err = strconv.Atoi(line[i : i+1])
			fmt.Print(lineSlice[i])
			if err != nil {
				log.Fatal(err)
			}
		}
		fmt.Println()
		multiplier := 100000000000
		prevIndex := -1
		maxJolt := 0
		for i := len(digits) - 1; i >= 0; i-- {
			val, ind := maxSliceValue(lineSlice[prevIndex+1 : len(lineSlice)-i])
			prevIndex = ind + prevIndex + 1
			digits[i] = val
			maxJolt += multiplier * val
			multiplier /= 10
		}
		fmt.Printf("Max Jolt %d\n", maxJolt)
		result += maxJolt
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Result: %d\n", result)
}

func maxSliceValue(slice []int) (value int, index int) {
	for i, x := range slice {
		if x > value {
			value = x
			index = i
		}
	}
	return
}
