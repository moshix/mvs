package main

/*
to run tests:

    go test blocksize.go blocksize_test.go
*/

import "testing"

type blockSizeTestCase struct {
	dasdName          string
	lrecl             int
	expectedBlockSize int
	shouldError       bool
}

var btcs = []*blockSizeTestCase{
	{
		dasdName:          "2311",
		lrecl:             10,
		expectedBlockSize: 1810,
		shouldError:       false,
	},
	{
		dasdName:    "i don't exist",
		lrecl:       0,
		shouldError: true,
	},
	{
		dasdName:          "3390",
		lrecl:             23,
		expectedBlockSize: 28313,
		shouldError:       false,
	},
}

func Test_getBlockSize(t *testing.T) {
	for _, btc := range btcs {
		actual, err := getBlockSize(btc.dasdName, btc.lrecl)
		switch {
		case err != nil && !btc.shouldError:
			t.Fatalf("unexpected error in %v: %v", btc, err)
		case err == nil && btc.shouldError:
			t.Fatalf("missing error in %v", btc)
		case err != nil && btc.shouldError:
		default:
			if actual != btc.expectedBlockSize {
				t.Fatalf("incorrect block size in %v: %d", btc, actual)
			}
		}

	}
}
