package main

func main() {
	a := make(chan struct{})
	go func() {
		a <- struct{}{}
	}()
	go func() {
		<-a
	}()
}
