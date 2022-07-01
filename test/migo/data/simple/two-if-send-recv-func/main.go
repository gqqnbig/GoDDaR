package main

func send(a chan struct{}) {
	if true {
		a <- struct{}{}
	} else {
		<-a
	}
}
func main() {
	a := make(chan struct{})
	send(a)
	send(a)
}
