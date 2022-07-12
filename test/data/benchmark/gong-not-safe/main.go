package main

func recv(ch chan int) {
	<- ch
	close(ch)
}

func main() {
	ch := make(chan int)
  	go recv(ch)
	ch <- 0
	close(ch)
}
