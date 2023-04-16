#!/bin/bash
# Macos all addresses parser
# Copyright 2023 by Moshix - All right reserved
# You may only execute this script but not redistribute or enhance without first including the full 
#  script in its entirety as comment in your own changes or enhancements
# v 0.1 Humble beginnings
# v 0.2 Exclude non IP4 interfaces

for counter in 0 1 2 3 4 5 6 ;
do
   result=`ifconfig vmenet"$counter" 2>/dev/null`
   if grep -q "inet " <<< "$result"; then
      echo -n -e "NIC vmenet$counter:\t"
      ifconfig vmenet"$counter" |  grep "inet " | awk '{print $2}'
   fi
done


for counter in 0 1 2 3 4 5 6;
do
   result=`ifconfig en"$counter" 2>/dev/null`
   if grep -q "inet " <<< "$result"; then
      echo -n -e "NIC en$counter:\t"
      ifconfig en"$counter" |  grep "inet " | awk '{print $2}'
   fi
done



for counter in 0 1 2 3 4 5 6;
do
   result=`ifconfig utun"$counter" 2>/dev/null`
   if grep -q "inet " <<< "$result"; then
      echo -n -e "NIC utun$counter:\t"
      ifconfig utun"$counter" |  grep "inet " | awk '{print $2}' 
   fi
done


for counter in 100 101 102 103 104;
do
   result=`ifconfig bridge"$counter" 2>/dev/null`
   if grep -q "inet " <<< "$result"; then
      echo -n -e "NIC bridge$counter:\t"
      ifconfig bridge"$counter" |  grep "inet " | awk '{print $2}'
   fi
done
