package main

func main() {
	a := make(chan struct{}, 0)
	if true {
		a <- struct{}{}
	} else {
		<-a
	}
}
