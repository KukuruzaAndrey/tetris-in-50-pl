{
if (((NR - 1) % 25 == 0) || ((NR - 2) % 25 == 0)) # lines with arguments (not render results)
    if ((($3 == 0) && ($7 < -2)) || # cases offsetY below that init-states offsetY
        (($3 == 1) && ($7 < -1)) ||
        (($3 == 2) && ($7 < -1)) ||
        (($3 == 3) && ($7 < -1)) ||
        (($3 == 4) && ($7 < -1)) ||
        (($3 == 5) && ($7 < 0)) ||
        (($3 == 6) && ($7 < -1))) print FILENAME, FNR
}