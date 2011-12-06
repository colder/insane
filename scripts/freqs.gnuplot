reset
unset title
set terminal png
set output "freqs.png"
fname2 = 'datano1'

bin(x, s) = s*int(x/s)
bw = 0.1
set boxwidth bw*0.6
set style fill solid 0.4
 
set title "Targets Refinement"
set xrange [0:1]
set xtics add (0,1)
set mxtics 0.05
set yrange [0:*]
set xlabel "Reduction"
set ylabel "Call Sites"
 
plot fname2 using (bin(($2-$1)/$2,bw)+bw/2):(1.0) smooth frequency ti 'freq' w boxes
