package main

func main() {
	a := make(chan struct{})
	b := make(chan struct{})
	go func() {
		<- b
		a <- struct{}{}
	}()
	<- a
	b <- struct{}{}
}
