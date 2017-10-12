#!/bin/sh

if [ -z "$EPSTOPDF" ]
then
    #EPSTOPDF=/usr/bin/epstopdf 
    EPSTOPDF=/home/acm22/bin/epstopdf.pl
fi
export PATH=$PATH@PATHEXT@

if [ ! -r "@SMLDIR@/bin/.run/run.@ARCH@" ]
then
    echo 1>&2 "@SMLDIR@ is not the SML directory"
    exit 1
fi
dir="@EXECDIR@"
smldir="@SMLDIR@"
machinetype="@ARCH@"
if [ ! -r "@EXECDIR@/relplot.@ARCH@" ]
then
    echo 1>&2 "Can't find relplot.@ARCH@ in @EXECDIR@"
    exit 1
fi

output="$1"

if [ "$2" = "yes" ]
then
    uniform_scale=true
else
    uniform_scale=false
fi
if [ "$3" = "yes" ]
then
    hide_axes=true
else
    hide_axes=false
fi
if [ "$4" = "yes" ]
then
    grid_lines=true
else
    grid_lines=false
fi


(
cat "$dir/relplot.ps"
echo "/uniform_scale $uniform_scale store"
echo "/hide_axes $hide_axes store"
echo "/grid_lines $grid_lines store"

shift #output
shift #aspect
shift #axes
shift #grid_lines

# echo running \
# "$smldir"/run.$machinetype @SMLload="$dir/relplot.$machinetype" "$@" \
    # >>logs/requests 
"$smldir"/bin/.run/run.$machinetype @SMLload="$dir/relplot.$machinetype" "$@"

echo 'showpage'
echo '%%EOF'
) | (
    if [ "$output" = ps ]
    then
        cat
    else
        "@EPSTOPDF@" --filter
    fi
)
