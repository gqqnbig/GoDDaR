package main

func main() {
	a := make(chan struct{})
	b := make(chan struct{})
	go func() {
		a <- struct{}{}
		<- b
	}()
	b <- struct{}{}
	<- a
}
