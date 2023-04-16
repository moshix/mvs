#!/bin/bash
# Macos all addresses parser
# run with ./nicparser.bash, for example in your .bash_profile
#
# Copyright 2023 by Moshix - All right reserved
# You may only execute this script but not redistribute or enhance without first including the full 
#  script in its entirety as comment in your own changes or enhancements
# v 0.1 Humble beginnings
# v 0.2 Exclude non IP4 interfaces
# v 0.3 Color!
# v 0.4 Generalized version for MacOs and Linux
# v 0.5 Nicer output
# v 0.6 Gets your external IP or timesout telling you no external IP


# terminal handling globals
red=`tput setaf 1`
green=`tput setaf 2`
yellow=`tput setaf 3`
blue=`tput setaf 4`
magenta=`tput setaf 5`
cyan=`tput setaf 6`
white=`tput setaf 7`
blink=`tput blink`
rev=`tput rev`
reset=`tput sgr0`
# echo "${red}red text ${green}green text${reset}"

#echo "These are your IPv4 addresses on your system"

for nictype in lo en utun bridge docker tap tun ens eth
do 
  for counter in 0 1 2 3 4 5 6 100 101 102 103 104 160 161 162
     do
        result=`ifconfig $nictype$counter 2>/dev/null`
        if grep -q "inet " <<< "$result"; then #NIC exists...
           echo -n -e "${blue}$nictype$counter:     \t${reset}"
           ifconfig "$nictype$counter" |  grep "inet " | awk 'BEGIN{ORS=""}{print $2}' # ORS controls new line
           echo  -e "${reset}"
        fi
     done
done
# now lets get external IP or timeout
echo -e "${blue}External IP: \t${white}"`timeout 2.3 curl ifconfig.me 2>/dev/null || echo "${red}no external IP${reset}"`
