directions=( "up" "down" "right" "left")
colors=( "red" "blue" "green" "black")

for i in "${directions[@]}"
do
  for j in "${colors[@]}"
  do
    convert fire.${i}.png -fuzz 15% -fill $j -opaque red ${j}.${i}.png
  done
done

