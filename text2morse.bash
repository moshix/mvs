#!/bin/bash
# copyright by moshix 2024
# text to morse
# pass it an input text file

# Check if a file name is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 filename"
    exit 1
fi

# Morse code definitions
declare -A morse=(
    [A]=".-"     [B]="-..."   [C]="-.-."   [D]="-.."
    [E]="."      [F]="..-."   [G]="--."    [H]="...."
    [I]=".."     [J]=".---"   [K]="-.-"    [L]=".-.."
    [M]="--"     [N]="-."     [O]="---"    [P]=".--."
    [Q]="--.-"   [R]=".-."    [S]="..."    [T]="-"
    [U]="..-"    [V]="...-"   [W]=".--"    [X]="-..-"
    [Y]="-.--"   [Z]="--.."
    [1]=".----"  [2]="..---"  [3]="...--"  [4]="....-"
    [5]="....."  [6]="-...."  [7]="--..."  [8]="---.."
    [9]="----."  [0]="-----"
)


beep_morse() {
    local char="$1"
    case "$char" in
        ".")
            # Dot: Short beep
            beep -l 100 -f 800
            #echo -n "$char"
            ;;
        "-")
            # Dash: Longer beep
            beep -l 300 -f 800
            ;;
    esac
    # Pause between parts of the same letter
    sleep 0.1
}

# Read the file
while IFS= read -r line; do
    # Convert line to uppercase (Morse is case-insensitive)
    line=$(echo "$line" | tr '[:lower:]' '[:upper:]')
    # Process each character
    for (( i=0; i<${#line}; i++ )); do
        char="${line:$i:1}"
        if [[ ${morse[$char]+_} ]]; then
            # Translate character to Morse and beep
            for (( j=0; j<${#morse[$char]}; j++ )); do
                echo -n "${morse[$char]}  "
                #beep_char="${morse[$char]:$j:1}"
                #beep_morse "$beep_char"
            done
            # Pause between letters
            sleep 0.1
        fi
    done
    # Pause between lines
    echo -e "\n"
    sleep 0.3
done < "$1"
echo " "
