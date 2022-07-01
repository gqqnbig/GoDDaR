package main

func main() {
	a := make(chan struct{})
	select {
	case <-a:
		select {
		case <-a:
		case a <- struct{}{}:
		}
	case a <- struct{}{}:
	}
}
