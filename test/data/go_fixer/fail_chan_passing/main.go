package main

func fun(a chan chan string) {
	b := <- a
	b <- ""
}

func main() {
	a := make(chan chan string)
	b := make(chan string)
	a <- b
	go fun(a)
	<- b
}
