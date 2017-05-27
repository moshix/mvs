package main

/*
   blockfactor
   block size calculator for most IBM DASDs
   invoke wiht blockfactor DASD LRECL parameters


   (c) 2017 by moshix
   Program source is under Apache license             */

import (
	"flag"
	"fmt"
)
/* global variables section */



func main() {
	var lrecl int = 0 // initialize lrecl
	var dasd = "" //  initialize DASD type

	dasdPtr := flag.String("dasd", "33903", "DASD model")
	lreclPtr := flag.Int("lrecl", 80, "logical record size")
	helpPtr := flag.Bool("help", false, "help flag")
	flag.Parse()

	lrecl = *lreclPtr // assign argument to main variable lrecl
	dasd = *dasdPtr   // assign dasd type to main variable dasd

	if dasd == "" {
		fmt.Println("No DASD type entered. ")
		fmt.Println("pls enter DASD type or restart with -h for list of DASD types")
		fmt.Scan(&dasd)
	}
	if lrecl == 0 {

		fmt.Printf("\nlrecl command line argument is not included.\n")
		fmt.Println("Please enter lrecl size: ")
		fmt.Scan(&lrecl)
	}
	if *helpPtr {
		fmt.Println("Usage example:   ")
		fmt.Println("blockfactor -h -dasd=3380K -lrecl=80")
		fmt.Println(" ")
		fmt.Println("-h         show this help         ")
		fmt.Println("-dasd      dasd type (see table below)")
		fmt.Println("-lrecl=80  logical record lengt")
		fmt.Println("Possible DASD types:")
		fmt.Println("2311 3330 3340 3350 3370 3375 3380A 3380B 3380E 3380K 33901 33903")
		return
	}
    // build table with full track size (not half!)
    table := make(map[string]int)
    table["2311"] = 3625
    table["2314"] = 7294
    table["3330"] = 13030
    table["3340"] = 8368
    table["3350"] = 19069
    table["3375"] = 35616
    table["3380"] = 47476
    table["3390"] = 56664
    table["9345"] = 46456
/*  this is just an exampmle on how to print out table
    p.s. prints out in random order.... not sure why 
for model, size := range table {
         fmt.Println("Model",model, "size",size)
    } */



    // calculate optimum block size 
    fulltracks := table[dasd]  //obtain pair value of key dasd
    halftracks := fulltracks / 2 // half track size
    blocksize := int((halftracks / lrecl) * lrecl)
    fmt.Println("BLK100I Ideal blocksize for dasd ",dasd," and lrecl: ",lrecl," is: ",blocksize)   
	return // official exit of main back to OS
}
