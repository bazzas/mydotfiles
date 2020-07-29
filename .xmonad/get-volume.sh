# ~/bin/get-volume.sh
#!/bin/bash
# Get the maximum volume of any pulseaudio sink channel
# amixer get Master | egrep -o "[0-9]+%"
vol=$(amixer get Master | awk -F'[]%[]' '/%/ {if ($5 == "off") { print ((-$2)) } else { print $2 }}' | head -n 1)
if (($vol < 0)); then
(( res = 0 - vol ));echo 婢 $res%
elif (($vol == 0)); then
echo ﱝ $vol%
elif (( $vol < 30)); then
echo  $vol%
elif (( $vol < 60)); then
echo 墳 $vol%
else
echo  $vol%
fi
exit 0
