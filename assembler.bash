# this is the basic structure for an S/370 assembler in bash
# (c) 2024 by moshix
# can be easily extended
# works as fast as a python program
# v 0.1 humble beginnings
# v 0.2 first few instructions
# v 0.3 CSECT and DSECT 
#!/bin/bash

# Instruction set (extendable)
declare -A INSTRUCTIONS=(
    ["L"]="58"    # Load
    ["ST"]="50"   # Store
    ["A"]="5A"    # Add
    ["S"]="5B"    # Subtract
    ["M"]="5C"    # Multiply
    ["D"]="5D"    # Divide
    ["B"]="47"    # Branch
    ["BZ"]="47"   # Branch on Zero
    ["BN"]="47"   # Branch on Negative
    ["MVI"]="92"  # Move Immediate
    # Add more instructions here as needed
)

# Function to assemble a single line of S/370 assembly code
assemble_line() {
    local line="$1"
    local instruction=$(echo "$line" | awk '{print $1}')
    local operands=$(echo "$line" | awk '{print $2}')

    # Handle special directives
    case "$instruction" in
        "USING")
            echo "Processing USING directive: $operands"
            ;;
        "CSECT")
            echo "Processing CSECT directive: $operands"
            ;;
        "DSECT")
            echo "Processing DSECT directive: $operands"
            ;;
        *)
            # Check if the instruction exists
            if [[ -n "${INSTRUCTIONS[$instruction]}" ]]; then
                local opcode="${INSTRUCTIONS[$instruction]}"
                echo "$opcode $operands"
            else
                echo "Error: Unknown instruction '$instruction'" >&2
            fi
            ;;
    esac
}

# Main function to assemble a file
assemble_file() {
    local file="$1"

    if [[ ! -f "$file" ]]; then
        echo "Error: File '$file' not found." >&2
        return 1
    fi

    while IFS= read -r line; do
        assemble_line "$line"
    done < "$file"
}

# Check for input file argument
if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <assembly_file>" >&2
    exit 1
fi

assemble_file "$1"
