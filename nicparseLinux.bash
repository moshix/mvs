#!/bin/bash
# Linux  all IPv4  addresses parser
# Copyright 2023 by Moshix - All right reserved
# You may only execute this script but not redistribute or enhance without first including the full 
#  script in its entirety as comment in your own changes or enhancements
# v 0.1 Humble beginnings
# v 0.2 Exclude non IP4 interfaces
# v 0.3 Linux usual NICs as well as Hercules tap/tun/docker stuff

for counter in 0 1 ;
do
   result=`ifconfig docker"$counter" 2>/dev/null`
   if grep -q "inet " <<< "$result"; then
      echo -n -e "NIC docker$counter:\t"
      ifconfig docker"$counter" |  grep "inet " | awk '{print $2}'
   fi
done


for counter in 0 1 2 3 4 5 6 160 161 162 164
do
   result=`ifconfig ens"$counter" 2>/dev/null`
   if grep -q "inet " <<< "$result"; then
      echo -n -e "NIC ens$counter:\t"
      ifconfig ens"$counter" |  grep "inet " | awk '{print $2}'
   fi
done



for counter in 0 1 2 3 4 5 6;
do
   result=`ifconfig eth"$counter" 2>/dev/null`
   if grep -q "inet " <<< "$result"; then
      echo -n -e "NIC eth$counter:\t"
      ifconfig eth"$counter" |  grep "inet " | awk '{print $2}' 
   fi
done


for counter in 0 1 2 3 4 5;
do
   result=`ifconfig tap"$counter" 2>/dev/null`
   if grep -q "inet " <<< "$result"; then
      echo -n -e "NIC tap$counter:\t"
      ifconfig tap"$counter" |  grep "inet " | awk '{print $2}'
   fi
done

for counter in 0 1 2 3 4 ;
do
   result=`ifconfig tun"$counter" 2>/dev/null`
   if grep -q "inet " <<< "$result"; then
      echo -n -e "NIC tun$counter:\t"
      ifconfig tun"$counter" |  grep "inet " | awk '{print $2}'
   fi
done
