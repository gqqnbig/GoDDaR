package main

func main() {
	a := make(chan struct{})
	select {
	case <-a:
	case a <- struct{}{}:
	default:
	}
}
