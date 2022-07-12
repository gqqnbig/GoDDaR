package main

func sender(a chan int, x int) {
	for i := 0; i < x; i++ {
		a <- i
	}
}

func receiver(a chan int, x int) {
	for i := 0; i < x; i++ {
		<-a
	}
}

func main() {
	a := make(chan int)
	go sender(a, 3)
	receiver(a, 3)
}
