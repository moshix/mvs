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
# v 1.0 DNS check and user speed perception improvements
# v 1.1 Which NIC to internet
# v 1.2 Add more NIC types and handle loopback better
# v 1.3 Hanlde virtual NICS like ens160:1
# v 1.4 -n switch provides no color for better machine further passing in out (ie in pipe (

version="1.4"

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

os_type() {
case "$OSTYPE" in
      freebsd*) ostype=FreeBSD;;
      win32*)   ostype=Windows;;
      linux*)   ostype=Linux;;
      darwin*)  ostype=Mac;;
      *)        ostype="UNKNOWN"
esac
#echo ${ostype}
}

# main loop here
if [[ "$1" != "-n" ]]; then
   set_color  # user didnt' specify no color
fi

if [[ "$1" == "-h" ]]; then
     echo "-e 1.5 to get your external IP with timeout of 1.5 secs"
     echo "-n to suppress color " 
     echo "-v to show the version" 
     echo "-h to show this help" 
     exit 0
fi

os_type # find out which OS

# in Linux the loopback is just lo without a number
if [[ "$ostype" == "Linux" ]]; then
   result=`ifconfig lo  2>/dev/null`
   if grep -q "inet " <<< "$result"; then #NIC exists...
           echo -n -e "${blue}lo:     \t\t\t${reset}"
           ifconfig lo |  grep "inet " | awk 'BEGIN{ORS=""}{print $2}'
           echo  -e "${reset}"
   fi
fi


for nictype in lo en  # this gives the user the impression that the search for IPs is taking long, not the external IP check
do
  for counter in 0 1 2 3 4 5 6 100 101 102 103 104 160 161 162
     do
        result=`ifconfig $nictype$counter 2>/dev/null`
        if grep -q "inet " <<< "$result"; then #NIC exists...
           echo -n -e "${blue}$nictype$counter:     \t\t${reset}"
           ifconfig "$nictype$counter" |  grep "inet " | awk 'BEGIN{ORS=""}{print $2}' # ORS controls new line
           echo  -e "${reset}"
        fi
     done
done


if [[ "$1" == "-e" ]] || [[ "$1" == "-v" ]] || [[ "$1" == "-n" ]]; then
   if [[ -z "$2" ]]; then
      delay=1.4
      ext=`timeout $delay curl ifconfig.me 2>/dev/null`  
   else
      delay=$2
      ext=`timeout $delay curl ifconfig.me 2>/dev/null` 
   fi
fi

dnscheck=`dig -t srv www.google.com`

for nictype in  wlan enp3s wlp2s utun bridge docker tap tun ens eth vde-dnet-tap inettap lxcbr
do 
  for counter in 0 1 2 3 4 5 6 100 101 102 103 104 160 161 162
     do
        result=`ifconfig $nictype$counter 2>/dev/null`
        if grep -q "inet " <<< "$result"; then #NIC exists...
           echo -n -e "${blue}$nictype$counter:     \t\t${reset}"
           ifconfig "$nictype$counter" |  grep "inet " | awk 'BEGIN{ORS=""}{print $2}' # ORS controls new line
           echo  -e "${reset}"
           for virtual in :0 :1 :2 :3 :4 :5 :6 :6
              do
                resultvirt=`ifconfig $nictype$counter$virtual 2>/dev/null`
                if grep -q "inet " <<< "$resultvirt"; then #virtual NIC exists...
                   echo -n -e "${blue}$nictype$counter$virtual:     \t\t${reset}"
                   ifconfig "$nictype$counter$virtual" |  grep "inet " | awk 'BEGIN{ORS=""}{print $2}' # ORS controls new line
                   echo  -e "${reset}"
                fi
               done 
        fi
     done
done

if [[ -z "$ext" ]]; then
    echo -e "External IP: \t\t${red}no internet connection - or delay too short${reset}" # the other thread definetely not done yet
else
   echo -e "${blue}External IP: \t\t${white}$ext ${reset}"

fi


if [[ "$ostype" == "Linux" ]]; then
 routenic=`route | grep '^default' | grep -o '[^ ]*$'`  # for Linux
 echo -e "${blue}NIC to Internet: \t${white}$routenic${reset}"
fi


if [[ "$ostype" == "Mac" ]]; then
 routenic=`route -n get default | grep 'interface:' | grep -o '[^ ]*$'`
 echo -e "${blue}NIC to Internet: \t${white}$routenic${reset}"
fi

if [[ -z "$dnscheck" ]]; then
    sleep 0.2 # this gives external ip a bit more time
else
   echo -e "${blue}DNS config:  \t\t${white}OK ${reset}"
fi

if [[ "$1" == "-v" ]]; then
      echo -e "${cyan}Version:\t\t"${white}$version"${reset}"
fi
