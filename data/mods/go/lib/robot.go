package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const (
	laby_name_Void    = "Void"
	laby_name_Wall    = "Wall"
	laby_name_Rock    = "Rock"
	laby_name_Web     = "Web"
	laby_name_Exit    = "Exit"
	laby_name_Unknown = "Unknown"
)

func input() string {
	reader := bufio.NewReader(os.Stdin)
	text, _ := reader.ReadString('\n')
	input := strings.TrimSpace(text)

	if input == "quit" {
		os.Exit(0)
	}

	return input
}

func laby_name_left() {
	fmt.Println("left")
	input()
}

func laby_name_right() {
	fmt.Println("right")
	input()
}

func laby_name_forward() {
	fmt.Println("forward")
	input()
}

func laby_name_take() {
	fmt.Println("take")
	input()
}

func laby_name_drop() {
	fmt.Println("drop")
	input()
}

func laby_name_escape() {
	fmt.Println("escape")
	input()
}

func laby_name_say(s string) {
	fmt.Println("say " + s)
	input()
}

func laby_name_look() string {
	fmt.Println("look")

	ans := input()

	switch ans {
	case "void":
		return laby_name_Void
	case "wall":
		return laby_name_Wall
	case "rock":
		return laby_name_Rock
	case "web":
		return laby_name_Web
	case "exit":
		return laby_name_Exit
	default:
		return laby_name_Unknown
	}
}

func init() {
	fmt.Println("start")
	input()
}
