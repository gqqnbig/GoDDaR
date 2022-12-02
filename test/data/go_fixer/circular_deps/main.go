package main

func main() {
	a := make(chan string)
	b := make(chan string)
	go func() {
		tmp := <- b
		a <- tmp
	}()
	<- a
	b <- "DATA"
}
