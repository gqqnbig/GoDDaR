package main

import (
	"fmt"
	"time"
)

func work(data int) {
	for {
		fmt.Println("Working!")
		time.Sleep(1 * time.Second)
	}
}

func recvr(ch <-chan int, done chan<- int) {
	val := <-ch
	go work(val)
	done <- val
}

func sender(ch chan<- int) {
	ch <- 42
}

func main() {
	ch, done := make(chan int), make(chan int)
	go recvr(ch, done)
	go sender(ch)
	go sender(ch)
	<-done
	<-done
}
