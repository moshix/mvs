package main
// This is initial code by Matt Wilson. I may make this into a Linux tn3270 server for HNET
import (
	"bytes"
	"fmt"
	"net"
)

const (
	iac  byte = 255
	dont byte = 254
	do   byte = 253
	wont byte = 252
	will byte = 251
	sb   byte = 250
	se   byte = 240
	eor  byte = 239

	opttermtype byte = 0xf0
	opteor      byte = 0x19
	optbin      byte = 0
)

var codes = []byte{0x40, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8,
	0xc9, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0xd1, 0xd2, 0xd3, 0xd4,
	0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 0x60,
	0x61, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0x6a, 0x6b, 0x6c,
	0x6d, 0x6e, 0x6f, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
	0xf9, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f}

var ebcdic = []byte{
	0, 1, 2, 3, 55, 45, 46, 47, 22, 5, 37, 11, 12, 13, 14, 15, 16, 17, 18, 19,
	60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31, 64, 79, 127, 123, 91, 108,
	80, 125, 77, 93, 92, 78, 107, 96, 75, 97, 240, 241, 242, 243, 244, 245,
	246, 247, 248, 249, 122, 94, 76, 126, 110, 111, 124, 193, 194, 195, 196,
	197, 198, 199, 200, 201, 209, 210, 211, 212, 213, 214, 215, 216, 217, 226,
	227, 228, 229, 230, 231, 232, 233, 74, 224, 90, 95, 109, 121, 129, 130,
	131, 132, 133, 134, 135, 136, 137, 145, 146, 147, 148, 149, 150, 151, 152,
	153, 162, 163, 164, 165, 166, 167, 168, 169, 192, 106, 208, 161, 7, 32,
	33, 34, 35, 36, 21, 6, 23, 40, 41, 42, 43, 44, 9, 10, 27, 48, 49, 26, 51,
	52, 53, 54, 8, 56, 57, 58, 59, 4, 20, 62, 225, 65, 66, 67, 68, 69, 70, 71,
	72, 73, 81, 82, 83, 84, 85, 86, 87, 88, 89, 98, 99, 100, 101, 102, 103,
	104, 105, 112, 113, 114, 115, 116, 117, 118, 119, 120, 128, 138, 139, 140,
	141, 142, 143, 144, 154, 155, 156, 157, 158, 159, 160, 170, 171, 172, 173,
	174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188,
	189, 190, 191, 202, 203, 204, 205, 206, 207, 218, 219, 220, 221, 222, 223,
	234, 235, 236, 237, 238, 239, 250, 251, 252, 253, 254, 255}

var ascii = []byte{
	0, 1, 2, 3, 156, 9, 134, 127, 151, 141, 142, 11, 12, 13, 14, 15,
	16, 17, 18, 19, 157, 133, 8, 135, 24, 25, 146, 143, 28, 29, 30, 31,
	128, 129, 130, 131, 132, 10, 23, 27, 136, 137, 138, 139, 140, 5, 6, 7,
	144, 145, 22, 147, 148, 149, 150, 4, 152, 153, 154, 155, 20, 21, 158, 26,
	32, 160, 161, 162, 163, 164, 165, 166, 167, 168, 91, 46, 60, 40, 43, 33,
	38, 169, 170, 171, 172, 173, 174, 175, 176, 177, 93, 36, 42, 41, 59, 94,
	45, 47, 178, 179, 180, 181, 182, 183, 184, 185, 124, 44, 37, 95, 62, 63,
	186, 187, 188, 189, 190, 191, 192, 193, 194, 96, 58, 35, 64, 39, 61, 34,
	195, 97, 98, 99, 100, 101, 102, 103, 104, 105, 196, 197, 198, 199, 200, 201,
	202, 106, 107, 108, 109, 110, 111, 112, 113, 114, 203, 204, 205, 206, 207, 208,
	209, 126, 115, 116, 117, 118, 119, 120, 121, 122, 210, 211, 212, 213, 214, 215,
	216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231,
	123, 65, 66, 67, 68, 69, 70, 71, 72, 73, 232, 233, 234, 235, 236, 237,
	125, 74, 75, 76, 77, 78, 79, 80, 81, 82, 238, 239, 240, 241, 242, 243,
	92, 159, 83, 84, 85, 86, 87, 88, 89, 90, 244, 245, 246, 247, 248, 249,
	48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 250, 251, 252, 253, 254, 255}

func main() {
	ln, err := net.Listen("tcp", ":3270")
	if err != nil {
		panic(err)
	}
	for {
		conn, err := ln.Accept()
		if err != nil {
			panic(err)
		}
		go handle(conn)
	}
}

func handle(conn net.Conn) {
	defer conn.Close()
	rbuf := make([]byte, 255)

	conn.Write([]byte{0xff, 0xfd, 0x18}) // DO TermType
	conn.Read(rbuf)
	conn.Write([]byte{0xff, 0xfa, 0x18, 0x01, 0xff, 0xf0}) // TermType suboptions
	conn.Read(rbuf)
	conn.Write([]byte{0xff, 0xfd, 0x19}) // DO EOR
	conn.Read(rbuf)
	conn.Write([]byte{0xff, 0xfd, 0x00}) // DO Binary
	conn.Read(rbuf)

	conn.Write([]byte{0xff, 0xfb, 0x19, 0xff, 0xfb, 0x00}) // WILL binary, eor
	conn.Read(rbuf)

	var b bytes.Buffer

	b.Write([]byte{0xf5, 0xc3})
	b.Write(sba(2, 11))
	b.Write(sf(prot))
	b.Write(a2e("Sign-on Procedure"))
	b.Write(sba(3, 2))
	b.Write(sf(prot))
	b.Write(a2e("Please enter your sign-on information"))
	b.Write(ra(4, 1, 4, 40, '-'))
	b.Write(sba(6, 1))
	b.Write(sf(prot + hiint))
	b.Write(a2e("Name:"))
	b.Write(sf(0))
	b.Write([]byte{0x13})
	b.Write(sba(6, 25))
	b.Write(sf(prot + hiint))
	b.Write(a2e("Location:"))
	b.Write(sf(0))
	b.Write(sba(7, 1))
	b.Write(sf(prot + hiint))
	b.Write(a2e("Serial Number:"))
	b.Write(sf(numer))
	b.Write(sba(7, 23))
	b.Write(sf(prot))

	b.Write([]byte{0xff, 0xef})

	conn.Write(b.Bytes())

	for {
		n, err := conn.Read(rbuf)
		if err != nil {
			break
		}
		for i := 0; i < n; i++ {
			fmt.Printf("%c", ascii[rbuf[i]])
		}
		fmt.Printf("\n")
	}

}

func getpos(row, col int) []byte {
	result := make([]byte, 2)
	address := (row-1)*80 + (col - 1)
	hi := (address & 0xfc0) >> 6
	lo := address & 0x3f
	result[0] = codes[hi]
	result[1] = codes[lo]
	return result
}

func sba(row, col int) []byte {
	result := make([]byte, 1, 3)
	result[0] = 0x11
	result = append(result, getpos(row, col)...)
	return result
}

func sf(attr int) []byte {
	result := make([]byte, 2)
	result[0] = 0x1d
	result[1] = codes[attr]
	return result
}

func a2e(a string) []byte {
	result := make([]byte, len(a))
	for i := 0; i < len(a); i++ {
		result[i] = ebcdic[a[i]]
	}
	return result
}

func ra(row, col, trow, tcol int, char byte) []byte {
	result := make([]byte, 0, 7)
	result = append(result, sba(row, col)...)
	result = append(result, 0x3c)
	result = append(result, getpos(trow, tcol)...)
	result = append(result, ebcdic[char])
	return result
}

const (
	prot  = 1 << 5
	numer = 1 << 4
	hiint = 1 << 3
	mdt   = 1
)
