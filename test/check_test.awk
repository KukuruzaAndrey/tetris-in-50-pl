{
    if (((NR - 1) % 25 == 0) || ((NR - 2) % 25 == 0)) { # lines with arguments (not render results)
        if ((($3 == 0) && ($7 < -2)) || # if cases offsetY below that init-states offsetY -> bad test
            (($3 == 1) && ($7 < -1)) ||
            (($3 == 2) && ($7 < -1)) ||
            (($3 == 3) && ($7 < -1)) ||
            (($3 == 4) && ($7 < -1)) ||
            (($3 == 5) && ($7 < 0)) ||
            (($3 == 6) && ($7 < -1))) print FILENAME, ":", FNR, " offsetY below init-state"
        if (length($2) != 200) print FILENAME, ":", FNR, " board must be length of 200"
    }
}