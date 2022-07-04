package main

func main() {
	a, b := make(chan struct{}), make(chan struct{})
	go func() {
		a <- struct{}{}
	}()
	go func() {
		<- b
	}()

	select {
	case <- a:
		b <- struct{}{}
	case <- b:
		a <- struct{}{}
	}
}
