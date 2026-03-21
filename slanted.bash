#!/usr/bin/env bash
# slanted.bash — HASP-style block letter renderer
# All rights reserved 2026 by moshhix
# Adapted from IBM HASP Print/Punch Service BLKPRT routine.
# The mainframe assembly works as follows:
#
#   1. Each letter is stored as 12 rows of 2-byte (16-bit) masks in BLOCKA table
#   2. For each output line (line counter 0,2,4,...,22 = 12 lines):
#      For each character in the text:
#        - Load 2-byte mask for current row
#        - BLKLOOP: shift mask left (ALR R15,R15), if carry (high bit was 1),
#          print the character; otherwise leave blank
#        - Loop 12 times for 12 columns per block letter
#   3. Slant: position += 6, then subtract (line_counter / 2)
#      This shifts top rows right and bottom rows left, creating italic effect
#   4. 2 blank columns between each block letter, up to 8 characters
#
# This bash script follows the same approach using bitmask arrays.
#
# Usage: ./slanted.bash "WORD"

FONT_HEIGHT=12
CHAR_WIDTH=12

# BLOCKA equivalent: font bitmasks
# 12 hex values per character (one per row), representing 12-bit patterns
# Bit 11 (0x800) = leftmost column, bit 0 (0x001) = rightmost column
# Where a bit is 1, print the character itself; where 0, print a space.
declare -A GLYPH

GLYPH[" "]="000 000 000 000 000 000 000 000 000 000 000 000"
GLYPH["A"]="1FE 303 303 303 3FE 3FE 303 303 303 303 303 303"
GLYPH["B"]="FFC FFC C03 C03 FFC FFC C03 C03 C03 C03 FFC FFC"
GLYPH["C"]="FFF FFF C03 C00 C00 C00 C00 C00 C00 C03 FFF FFC"
GLYPH["D"]="FFC FFC C03 C03 C03 C03 C03 C03 C03 C03 FFC FFC"
GLYPH["E"]="FFF FFF C00 C00 FF0 FF0 C00 C00 C00 C00 FFF FFF"
GLYPH["F"]="FFF FFF C00 C00 C00 FF0 FF0 C00 C00 C00 C00 C00"
GLYPH["G"]="7FE 7FE C03 C00 C00 C3F C3F C03 C03 C03 7FE 7FE"
GLYPH["H"]="C03 C03 C03 C03 FFF FFF C03 C03 C03 C03 C03 C03"
GLYPH["I"]="FFF FFF 060 060 060 060 060 060 060 060 FFF FFF"
GLYPH["J"]="3FF 3FF 003 003 003 003 C03 C03 C03 E03 7FE 3FC"
GLYPH["K"]="C03 C03 C06 C0C C30 FE0 FE0 C30 C0C C06 C03 C03"
GLYPH["L"]="C00 C00 C00 C00 C00 C00 C00 C00 C00 C00 FFF FFF"
GLYPH["M"]="C03 E07 F0F D9B CF3 C63 C03 C03 C03 C03 C03 C03"
GLYPH["N"]="E03 E03 F03 D83 CC3 C63 C33 C1B C0F C07 C03 C03"
GLYPH["O"]="7FE 7FE C03 C03 C03 C03 C03 C03 C03 C03 7FE 7FE"
GLYPH["P"]="FFF FFF C03 C03 FFF FFF C00 C00 C00 C00 C00 C00"
GLYPH["Q"]="7FE C03 C03 C03 C03 C03 C1B C0F C07 C03 7FF 7FF"
GLYPH["R"]="FFF FFF C03 C03 FFF FFE C30 C18 C0C C06 C03 C03"
GLYPH["S"]="7FE C03 C00 C00 7FC 7FC 003 003 C03 C03 7FE 7FE"
GLYPH["T"]="FFF FFF 060 060 060 060 060 060 060 060 060 060"
GLYPH["U"]="C03 C03 C03 C03 C03 C03 C03 C03 C03 C03 FFF FFF"
GLYPH["V"]="C03 606 30C 186 0CC 0CC 186 30C 606 3FC 1F8 0F0"
GLYPH["W"]="C03 C03 C63 CF3 DFB F0F E07 C03 C03 C03 C03 C03"
GLYPH["X"]="C03 C03 606 30C 1F8 1F8 30C 606 C03 C03 C03 C03"
GLYPH["Y"]="C03 C03 606 30C 1F8 0F0 060 060 060 060 060 060"
GLYPH["Z"]="FFF 003 006 00C 018 030 060 0C0 180 300 FFF FFF"
GLYPH["0"]="7FE 7FE C0F C1B C33 C63 CC3 D83 F03 E03 7FE 3FC"
GLYPH["1"]="060 0E0 1E0 060 060 060 060 060 060 060 FFC FFC"
GLYPH["2"]="7FE C03 C03 003 00E 030 0C0 300 C00 C00 FFC FFC"
GLYPH["3"]="7FE C03 003 003 1FE 1FE 003 003 C03 C03 7FE 7FE"
GLYPH["4"]="C03 C03 C03 C03 FFF FFF 003 003 003 003 003 003"
GLYPH["5"]="FFF C00 C00 C00 FFC FFC 003 003 C03 C03 7FE 7FE"
GLYPH["6"]="7FE C03 C00 C00 FFC FFF C03 C03 C03 C03 7FE 7FE"
GLYPH["7"]="FFC FFC 003 006 00C 018 030 060 0C0 180 300 300"
GLYPH["8"]="7FE C03 C03 C03 7FE 7FE C03 C03 C03 C03 7FE 7FE"
GLYPH["9"]="7FE C03 C03 C03 7FE 7FE 003 003 C03 C03 7FE 7FE"
GLYPH["-"]="000 000 000 000 FFF FFF 000 000 000 000 000 000"
GLYPH["."]="000 000 000 000 000 000 000 000 000 0F0 0F0 000"
GLYPH["!"]="0F8 0F8 0F8 0F8 0F8 0F8 0F8 0F8 000 000 0F8 0F8"
GLYPH["?"]="7FC C03 003 006 01C 030 030 000 000 030 030 000"

if [[ $# -eq 0 ]]; then
    echo "Usage: $0 \"WORD\""
    echo "Supports: A-Z  0-9  space  - . ! ?"
    exit 1
fi

word="${1^^}"   # uppercase

# BLKPRT rendering
# Like the assembly, we build each output line by scanning all characters
# and using bitmask shift-and-test to place characters or blanks.

echo ""
max_width=0

# BLKBLD: loop through 12 output lines (assembly: line counter 0,2,...,22)
for ((line=0; line<FONT_HEIGHT; line++)); do

# Slant calculation (assembly SKIP520 logic)
# Assembly: LA R5,6(,R5)    → start 6 positions right of center
#           SRL R4,1        → line_index = line_counter / 2
#           SLR R5,R4       → position -= line_index
# Net offset = 6 - line_index
# Normalized for terminal (min offset = 6-11 = -5, add 5):
    indent=$((FONT_HEIGHT - 1 - line))

    if ((indent > 0)); then
        printf -v lead "%${indent}s" ""
    else
        lead=""
    fi

    row_text="$lead"

# BLKLUP: loop through each character in the word
    for ((c=0; c<${#word}; c++)); do
        char="${word:$c:1}"
        # BLOCKTR equivalent: look up character in glyph table
        [[ -z "${GLYPH[$char]+x}" ]] && char=" "

        # Get the bitmask row for this character at this line
        read -ra masks <<< "${GLYPH[$char]}"
        mask=$((16#${masks[$line]}))

        # BLKLOOP: shift mask left and test high bit, 12 columns
        # Assembly: ALR R15,R15 (shift left); BC 12,SKIP530 (skip if no carry)
        #           MVC 0(1,R5),$POSTSAV (print char if carry)
        for ((bit=CHAR_WIDTH-1; bit>=0; bit--)); do
            if (( mask & (1 << bit) )); then
                # Carry set: print the text character itself ($POSTSAV)
                row_text+="${char}"
            else
                # No carry: leave blank
                row_text+=" "
            fi
        done

        # 2 blanks between blocks (assembly: LA R5,2(,R5))
        if ((c < ${#word} - 1)); then
            row_text+="  "
        fi
    done

    printf "%s\n" "$row_text"

# max width for stats
    trimmed="$row_text"
    while [[ "${trimmed: -1}" == " " ]] && [[ -n "$trimmed" ]]; do
        trimmed="${trimmed%?}"
    done
    len=${#trimmed}
    ((len > max_width)) && max_width=$len
done

echo ""
printf "Word: %-30s  Characters: %2d  Width: %d columns\n" \
    "\"$1\"" "${#1}" "$max_width"
