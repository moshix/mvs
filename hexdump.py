#!/opt/homebrew/bin/python3.10

# Copyright 2023 by moshix
# 
#
# hexdump.py a hex disassembler in ython
#         
#         
#         Usage: hexdump.py objectfile 
#                          dumps hex in groups of 16 bytes    
#         
#         
#         
#
#
# v0.1     Humble beginnings
# v0.2     open file and read into byte array data
# v0.3     extract program name from MVS object file at position 8..16
# v0.4     loop thru object starting from position 359 and print out hex program
# v0.5 
import sys

def hexdump(filename):
    with open(filename, 'rb') as f:
        counter = 0
        while True:
            chunk = f.read(16)
            if not chunk:
                break
            hexdata = ' '.join(f'{byte:02X}' for byte in chunk)
            textdata = ''.join(chr(byte) if 32 <= byte <= 126 else '.' for byte in chunk)
            print(f'{counter:08X}  {hexdata:<48}  {textdata}')
            counter += 16 

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print(f'Usage: {sys.argv[0]} <filename>')
        sys.exit(1)
    hexdump(sys.argv[1])
