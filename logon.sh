#!/bin/bash

# NAME: now
# PATH: $HOME/bin
# DESC: Display current calendar and time
# CALL: Called from terminal or ~/.bashrc
# DATE: Apr 6, 2017. Modified: Apr 10, 2017.

# NOTE: To display all available toilet fonts use this one-liner:
#       for i in ${TOILET_FONT_PATH:=/usr/share/figlet}/*.{t,f}lf; do j=${i##*/}; toilet -d "${i%/*}" -f "$j" "${j%.*}"; done

# calendar current month with today higlighted.
# colors 00=bright white, 31=red, 32=green, 33=yellow, 34=blue, 35=purple,
#        36=cyan, 37=white
printf "\033[36m"       # color 36=cyan
echo ""; cal;
printf "\033[00m"       # color bright white (default)
echo ""

tput sc                 # Save cursor position.
# Move up 9 lines
while [ $((++i)) -lt 10 ]; do tput cuu1; done
tput cuf 25             # Move 25 columns right

# Do we have the toilet package?
if hash toilet 2>/dev/null; then
    echo " "$(date +"%I:%M %P")" " | \
#        toilet -f metal --filter border > /tmp/terminal
        toilet -f future --filter border:metal  > /tmp/terminal
# Do we have the figlet package?
elif hash figlet 2>/dev/null; then
    echo $(date +"%I:%M %P") | figlet > /tmp/terminal
# else use standard font
else
    echo $(date +"%I:%M %P") > /tmp/terminal
fi

while IFS= read -r Time; do
    printf "\033[01;32m" # color green
    printf "$Time"
    tput cud1           # Up one line
    tput cuf 25         # Move 25 columns right
done < /tmp/terminal

tput rc                 # Restore saved cursor position.

exit 0
