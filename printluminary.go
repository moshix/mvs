/*
	Author of this utility: Matthew R. Wilson <mwilson@mattwilson.org>
	Date: March 13, 2022
	License: I release this into the public domain.

	This utilty reads the files from
	<https://github.com/chrislgarry/Apollo-11/tree/master/Luminary099>
	and prepares a file to print with Virtual1403.

	Note:
	  There appears to be a discrepancy between the MAIN.agc file,
	  which lists TRIM_GIMBAL_CNTROL_SYSTEM.agc as a filename, and the
	  name of the file in the directory, which is
	  TRIM_GIMBAL_CONTROL_SYSTEM.agc (control spelled out fully). To include
	  the contents of the file, either change the spelling in MAIN.agc or
	  rename the file on disk.

	  Additionally, the MAIN.agc refers to a file,
	  LAMBERT_AIMPOINT_GUIDANCE.agc, which on disk is named
	  GENERAL_LAMBERT_AIMPOINT_GUIDANCE.agc. Again, rename one or the other.
*/

package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

const mainFileName = "MAIN.agc"

func main() {
	if len(os.Args) != 2 {
		log.Fatalln("Must provide one argument: path to Luminary099 directory")
	}
	path := os.Args[1]

	stat, err := os.Stat(path)
	if err != nil {
		log.Fatalf("Couldn't open path `%s`:\n%v\n", path, err)
	}
	if !stat.IsDir() {
		log.Fatalf("Path `%s` is not a directory.\n", path)
	}

	if err := doIt(path, os.Stdout); err != nil {
		log.Fatalln(err)
	}
}

func doIt(path string, w io.Writer) error {
	files := make([]string, 0)

	toc, err := os.Open(filepath.Join(path, mainFileName))
	if err != nil {
		return err
	}
	defer toc.Close()

	buf := bufio.NewScanner(toc)
	for buf.Scan() {
		files = append(files, buf.Text())
	}
	if err := buf.Err(); err != nil {
		return err
	}

	// We'll make our own table of contents
	fmt.Fprintln(w, "T A B L E    O F    C O N T E N T S")
	fmt.Fprintln(w)
	for _, line := range files {
		if line[0] != '$' {
			continue
		}
		fmt.Fprintln(w, line)
	}

	// Now print each file
	fileRegex := regexp.MustCompile(`\$([A-Z0-9_\-]+)\.agc`)
	for _, line := range files {
		if line[0] != '$' {
			continue
		}
		match := fileRegex.FindStringSubmatch(line)
		if match == nil {
			continue
		}
		if err := doFile(path, match[1], w); err != nil {
			// we will just warn, not abort the whole process
			fmt.Fprintf(os.Stderr, "WARNING: %v\n", err)
		}
	}

	return nil
}

func doFile(path, file string, w io.Writer) error {
	f, err := os.Open(filepath.Join(path, file+".agc"))
	if err != nil {
		return err
	}
	defer f.Close()

	buf := bufio.NewScanner(f)

	ready := false
	for buf.Scan() {
		line := buf.Text()
		if !ready {
			// Skip past front matter text in each file
			if !strings.HasPrefix(line, "# Page ") {
				continue
			}
			ready = true
		}

		// Page break on "# Page ..." lines
		if strings.HasPrefix(line, "# Page ") {
			fmt.Fprintf(w, "\f")
			line = strings.TrimPrefix(line, "# Page ")
			fmt.Fprintf(w, "# %-100s Page %4s\n\n", file, line)
			continue
		}

		// Some lines have additional commentary (with some HTML formatting)
		// that weren't part of the original listings; strip them.
		if strings.HasPrefix(line, "## ") {
			continue
		}

		// Otherwise, pass line through
		fmt.Fprintln(w, line)
	}
	if err := buf.Err(); err != nil {
		return err
	}

	return nil
}
