package main

func main() {
	ch, ch1 := make(chan int), make(chan string)
	go func(ch chan int, ch1 chan string) {
		ch1 <- "msg"
		<- ch
	} (ch, ch1)
	ch <- 1
	<- ch1
}
