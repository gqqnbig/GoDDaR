package main

func send(a chan struct{}) {
	a <- struct{}{}
}

func recv(b chan struct{}) {
	<-b
}
func main() {
	a := make(chan struct{}, 0)
	send(a)
	recv(a)
}
