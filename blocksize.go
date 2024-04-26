package main

/*
   blocksize.go
   A block size calculator for most emulated IBM DASDs
   invoke with:
   blocksize DASD LRECL parameters


   (c) 2017-2024 by moshix
                                                 */

import (
	"flag"
	"fmt"
	"log"
)

func main() {

	var lrecl = 0 // initialize lrecl
	var dasd = "" //  initialize DASD type

	flag.StringVar(&dasd, "dasd", "", "DASD model. One of 2311 2314 3330 3340 3350 3375 3380 3390 9345")
	// default to no DASD
	flag.IntVar(&lrecl, "lrecl", 0, "logical record length")
	//default to zero lrecl

	flag.Usage = func() {
		fmt.Fprintln(flag.CommandLine.Output(), `BLK080I Usage example:
BLK080I blocksize -dasd 3380 -lrecl 80`)
		flag.PrintDefaults()
	}

	flag.Parse() //  this parses the command line arguments

	if dasd == "" { // no dasd type was input
		fmt.Println("\nBLK205R No DASD type entered. ")
		fmt.Println("BLK206R pls enter DASD type or restart with -h for list of DASD types")
		if _, err := fmt.Scan(&dasd); err != nil {
			log.Fatalf("could not scan DASD: %v", err)
		}
	}
	if lrecl == 0 { // no lrecl was input

		fmt.Printf("\nBLK201R lrecl command line argument is not included.\n")
		fmt.Println("BLK202R Please enter lrecl length: ")
		if _, err := fmt.Scan(&lrecl); err != nil {
			log.Fatalf("could not scan lrecl: %v", err)
		}

	}
	blockSize, err := getBlockSize(dasd, lrecl)
	if err != nil {
		log.Fatalf("could not get blockSize: %v", err)
	}

	fmt.Println("\nBLK100I Ideal block size for DASD type", dasd, ", with LRECL ", lrecl, " is: ", blockSize)
	fmt.Println("BLK900I END OF PROCESSING")
}
func getBlockSize(dasd string, lrecl int) (int, error) {
	// build table with full track size
	dasds := []*DASD{
		{"2311", 3625},
		{"2314", 7294},
		{"3330", 13030},
		{"3340", 8368},
		{"3350", 19069},
		{"3375", 35616},
		{"3380", 47476},
		{"3390", 56664},
		{"9345", 46456},
	}
	table := make(map[string]int)
	for _, dasd := range dasds {
		table[dasd.Name] = dasd.TrackSize
	}
	/*  this is just an example of how to print out table
	   p.s. prints out in random order.
	For an explanation, read https://go.dev/blog/maps -> iteration order
	    	for model, size := range table {
		         fmt.Println("Model",model, "size",size)
		    } */

	/* formula:  BLOCKSIZE = INT(half of TRKSIZE/LRECL) * LRECL */

	tracks, ok := table[dasd]
	if !ok {
		return 0, fmt.Errorf("unknown DASD model: %s", dasd)
	}

	halfTracks := tracks / 2 // half track size
	blockSize := (halfTracks / lrecl) * lrecl

	return blockSize, nil
}

type DASD struct {
	Name      string
	TrackSize int
}

func (d *DASD) String() string {
	return fmt.Sprintf("[Name: %8s, Track-Size: %8d]", d.Name, d.TrackSize)
}
