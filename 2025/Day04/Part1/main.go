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
	grid := [][]rune{}
	result := 0

	for scanner.Scan() {
		row := make([]rune, len(scanner.Text()))
		for i, char := range scanner.Text() {
			row[i] = char
		}
		grid = append(grid, row)
	}
	for i, row := range grid {
		for j, char := range row {
			if char == '.' {
				continue
			}
			adjacentRolls := 0
			if i > 0 {
				if j > 0 && isRollOfPaper(grid[i-1][j-1]) {
					adjacentRolls += 1
				}
				if isRollOfPaper(grid[i-1][j]) {
					adjacentRolls += 1
				}
				if j < len(grid[0])-1 && isRollOfPaper(grid[i-1][j+1]) {
					adjacentRolls += 1
				}
			}
			if j > 0 && isRollOfPaper(grid[i][j-1]) {
				adjacentRolls += 1
			}
			if j < len(grid[0])-1 && isRollOfPaper(grid[i][j+1]) {
				adjacentRolls += 1
			}
			if i < len(grid)-1 {
				if j > 0 && isRollOfPaper(grid[i+1][j-1]) {
					adjacentRolls += 1
				}
				if isRollOfPaper(grid[i+1][j]) {
					adjacentRolls += 1
				}
				if j < len(grid[0])-1 && isRollOfPaper(grid[i+1][j+1]) {
					adjacentRolls += 1
				}
			}
			if adjacentRolls < 4 {
				grid[i][j] = 'x'
				result += 1
			}
		}
	}
	for _, str := range grid {
		fmt.Println(string(str))
	}
	fmt.Println(result)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}

func isRollOfPaper(ch rune) bool {
	return ch == '@' || ch == 'x'
}
