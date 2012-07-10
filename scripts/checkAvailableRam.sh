ram=$(free | awk '/^Mem:/{print $4}')

if [ "$ram" -lt 200000 ]; then
    echo "Below 200M, killing..."
    pid=$(ps aux | grep insane.Main | grep bin/java | awk '{print $2}')
    echo "PID: $pid"
    kill $pid
else
    echo "Ram OK: $ram"
fi
