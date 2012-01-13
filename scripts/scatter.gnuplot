reset
unset title
set terminal png
set output "scatter.png"

f(x) = x
set title "Reductions"
set xlabel "Static Targets"
set ylabel "Computed Targets"
set key off
set grid
set logscale
set size square
set style line 1 lt 1 pt 3
set style line 2 lt 3 lw 1
plot [1:1500] [1:1500] "data" linestyle 1, f(x) linestyle 2
pause -1 "Hit any key to continue"

