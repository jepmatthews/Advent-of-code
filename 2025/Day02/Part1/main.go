package main

import (
	"bufio"
	"fmt"
	"log"
	"os"

	"github.com/golang-design/clipboard"
)

func main() {
	result := "0"
	f, err := os.Open("../input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)

	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	err := clipboard.Init()
	if err != nil {
		log.Fatal(err)
	}
}
