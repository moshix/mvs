package main

/* 2017 copyright by moshix
   with Apache license             */
import (
	"flag"
	"fmt"
	"time"
)

func ta(tamsec int) {
	n := 0
	for i := 0; i < 9999999; i++ {
		n++
		if n == 23 { //prime number so the two header loops don't meet too often
			n = 0
			fmt.Println("\n\nThread A                       Thread B                   ")
			fmt.Println("--------------                 ---------------")
		}
		fmt.Println("A Counter: ", i)
		time.Sleep(time.Duration(tamsec) * time.Millisecond)
	}
}
func tb(tbmsec int) {
	n := 0
	for i := 0; i < 9999999; i++ {
		n++
		if n == 19 { //prime number to avoid too many headers collisions
			n = 0
			fmt.Println("\n\nThread A                       Thread B                   ")
			fmt.Println("--------------                 ---------------")
		}
		fmt.Println("                               B Counter: ", i)
		time.Sleep(time.Duration(tbmsec) * time.Millisecond)
	}
}
func timeoutfunc(t chan bool, toutsec int) {
	time.Sleep(time.Duration(toutsec) * time.Second)
	t <- true
}

func main() {

	fmt.Println("Thread A                       Thread B                   ")
	fmt.Println("--------------                 ---------------")
	var tamsec int = 300
	var tbmsec int = 100
	var toutsec int = 15

	threadaPtr := flag.Int("tamsec", 300, "thread A millisecond wait time")
	threadbPtr := flag.Int("tbmsec", 100, "thread B millisecond wait time")
	toutPtr := flag.Int("tout", 15, "timeout period")
	helpPtr := flag.Bool("help", false, "help flag")
	flag.Parse()

	if *helpPtr { //user asked for help...
		fmt.Println("\nconcurrency is a Golang concurrency testing tool by moshix")
		fmt.Println("Three parameters are needed, and a flag is optional:")
		fmt.Println("-tamsec=nnn  hundreds of milliseconds for thread A to wait")
		fmt.Println("-tbmsec=nnn hundreds of milliseconds for thread B to wait")
		fmt.Println("-tout=nn  tens of seconds for time-out of all threads.")
		fmt.Println("-help for this help dialog")
		fmt.Println("\nconcurrency will in any case time out after 15 seconds")

		return
	}

	tamsec = *threadaPtr //assign argument of thread A wait time in msec
	tbmsec = *threadbPtr //assign argument of thread B wait time in msec
	toutsec = *toutPtr   // assign time out in seconds
	t := make(chan bool)
	go timeoutfunc(t, toutsec)

	go ta(tamsec)
	go tb(tbmsec)

	for {

		tend := <-t
		if tend == true {
			fmt.Println("time-out from control and timing thread!")
			return
		}

	}
}
