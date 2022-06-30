package main

func main() {
	a := make(chan struct{}, 0)
	<-a
}
