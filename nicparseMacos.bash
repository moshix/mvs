#!/usr/bin/env bash
# NIC addresses parser for Linux, AIX, Macos, Solaris
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
# v 0.6 Gets your external IP or times out telling you no external IP
# v 0.7 Get extenral IP optional with -e switch 
# v 0.8 Set time out in second argument to n.n seconds (e.g. 2.1) with 2.2, only if -e is present, (e.g. -e 1.2)
# v 0.9 Added more common Linux NICs
# v 1.0 Now with multi-threading, with semaphore

version="1.0"

set_color() {
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
}

get_external() {
semaphore=1
ext=`timeout $delay curl ifconfig.me 2>/dev/null`
semaphore=0
#echo "external value from curl: $external"

#echo -e "${blue}External IP: \t${white}"`timeout $delay curl ifconfig.me 2>/dev/null || echo "${red}no internet connection - or delay too short${reset}"`
}

# main loop here
if [[ "$1" == "-e" ]]; then
   if [[ -z "$2" ]]; then
      delay=1.4
      get_external & # call external IP routine
   else
      delay=$2
      get_external & 
   fi
else
     echo "Use the -e 1.2 switch to get your external IP with timeout 1.2 secs" # we don't have color yet, to make things faster
fi

set_color 


for nictype in lo wlan enp3s wlp2s en utun bridge docker tap tun ens eth vde-dnet-tap inettap 
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


if [[ "$semaphore" == 1 ]]; then
   sleep 0.4 # give it another chance
fi

if [[ "$semaphore" == 1 ]]; then
    echo -e "External IP: \t${red}no internet connection - or delay too short${reset}" # the other thread definetely not done yet
else
   echo -e "${blue}External IP: \t${white}"`timeout $delay curl ifconfig.me 2>/dev/null || echo "${red}no internet connection - or delay too short${reset}"`

fi


if [[ "$1" == "-v" ]]; then
      echo -e "${white}Version "$version"${reset}"
fi
