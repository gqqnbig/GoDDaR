package main

func main() {
	a := make(chan string, 1)
	go func() {
		a <- "2"
		<- a
	}() 
	a <- "1"
	<- a
}
