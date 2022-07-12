package main

func main() {
	a := make(chan struct{})
	if true {
		a <- struct{}{}
	}
	<-a
}
