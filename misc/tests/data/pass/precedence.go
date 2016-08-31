package main

func main() {
	// Equals 97.
	a := 45 + 52
	// Next line equivalent to: (23%45) + (21/45) + 5 - ((2<<3)*6)&5
	// Equals 28.
	b := 23%45 + 21/45 + 5 - 2<<3*6&5
}
