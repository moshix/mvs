package main
/* copyright 2020 by moshix
   crypto/rand number generator using /dev/random (depends on reliability of this Linux mechanism...
   or connect to Java library for atmospheric noise random number generator as in second example
*/

import (
	crand "crypto/rand"
	rand "math/rand"

	"encoding/binary"
	"fmt"
	"log"
	"os"
	"strconv"
)
// check the numbers.log file with dieharder entropy checker with
// dieharder -a -c ' ' -f numbers.log  where -c ' ' is the seperator between numbers
func main() {
    // check if numbers.log file is > 1MB delete it
    var numsize int64
    fileStat, err := os.Stat("numbers.log")
    if err != nil {
        log.Fatal(err)
    }
    numsize = fileStat.Size()
    if numsize > 1000000 {
       e := os.Remove("numbers.log") 
    if e != nil { 
        log.Fatal(e) 
    } 
    }

	f, err := os.OpenFile("numbers.log",
		os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0666)
	if err != nil {
		log.Println(err)
	}
	defer f.Close()
	var number int
	for i := 1; i < 14; i++ {
		var src cryptoSource
		rnd := rand.New(src)
		number = rnd.Intn(899) + 100
		s := strconv.Itoa(number)
		fmt.Print(number)
		fmt.Print(" ")
		if _, err := f.WriteString(s + " "); err != nil {
			log.Println(err)
		}
	}

	fmt.Print(" ..... ") // seperator for block on web page
      
	for i := 1; i < 14; i++ {
		var src cryptoSource
		rnd := rand.New(src)
		number = rnd.Intn(899) + 100
		s := strconv.Itoa(number)
		fmt.Print(number)
		fmt.Print(" ")
		if _, err := f.WriteString(s + " "); err != nil {
			log.Println(err)
		}
	}

}

type cryptoSource struct{}

func (s cryptoSource) Seed(seed int64) {}

func (s cryptoSource) Int63() int64 {
	return int64(s.Uint64() & ^uint64(1<<63))
}

func (s cryptoSource) Uint64() (v uint64) {
	err := binary.Read(crand.Reader, binary.BigEndian, &v)
	if err != nil {
		log.Fatal(err)
	}
	return v
}
