package main

func fun(c chan string) {
	a := make(chan string)
	go func() {
		a <- ""
	}()
	c <- (<- a)
}

func main() {
	a := make(chan string)
	b := make(chan string)
	go fun(a)
	go fun(b)
	<- a
	<- b
}
