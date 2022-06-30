package main

func send(a, b chan struct{}) {
	if true {
		a <- struct{}{}
	} else {
		<-b
	}
}
func main() {
	a := make(chan struct{})
	b := make(chan struct{})
	send(a, b)
	send(b, a)
}
