title="Moshix'  Wonderful Ascii World"
prompt="Pick an option:"
options=("Star Wars" "Acquarium" "Mission Impossible Computer Screen")
trap ctrl_c INT
function ctrl_c() {
        echo "** Trapped CTRL-C **"
}
today=$(date)
echo "$today" >> /home/asci/visitor.log
echo
echo
echo "$title"
PS3="$prompt "
select opt in "${options[@]}" "Quit"; do
    case "$REPLY" in
    1 ) /home/asci/starwars/asciimation.awk ./starwars/data/sw1.txt;;
    2 ) /usr/local/bin/asciiquarium;;
    3 ) /usr/bin/hollywood;;
    $(( ${#options[@]}+1 )) ) echo; echo; echo "        OK, back to your terminal prompt...Goodbye!"; sleep 2; break;;
    *) echo "Invalid option. Try another one.";continue;;
    esac
done
