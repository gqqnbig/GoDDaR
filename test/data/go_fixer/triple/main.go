package main

func main() {
	a := make(chan struct{})
	b := make(chan struct{})
	c := make(chan struct{})

	go func() {
		<-a
		b <- struct{}{}
		<-c
	}()

	c <- struct{}{}
	a <- struct{}{}
	<-b
}
