package main

func send(a chan struct{}) {
	a <- struct{}{}
}
func main() {
	a := make(chan struct{})
	send(a)
	send(a)
}
