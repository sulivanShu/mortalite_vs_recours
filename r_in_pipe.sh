#/bin/sh

mkfifo $XDG_DATA_HOME/fifo 2>/dev/null ; chmod 600 $XDG_DATA_HOME/fifo && tail -f $XDG_DATA_HOME/fifo | R --no-echo --save --restore --file=- & echo 'options(error=dump.frames)' | cat > $XDG_DATA_HOME/fifo ; fg 2>/dev/null
