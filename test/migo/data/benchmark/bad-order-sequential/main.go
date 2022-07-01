package main

func main() {
	a := make(chan int)
	b := make(chan int)
	c := make(chan int)
	
	go func(b chan int, c chan int) {
		b <- 1
		<-c
	} (b, c)
	
	go func(b chan int, c chan int) {
		<-b
		c <-2
	} (b,c)
	
	a <- 3
	<-a
}
