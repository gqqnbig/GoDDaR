package main

func main() {
	a := make(chan struct{})
	for {
		a <- struct{}{}
	}
}
