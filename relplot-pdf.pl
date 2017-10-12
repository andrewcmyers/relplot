#!/bin/sh
EPSTOPDF=/usr/bin/epstopdf 
export PATH=$PATH:/sw/bin

if test `uname` = Darwin
then
    smldir=/usr/local/smlnj/bin/.run
    dir=.
    machinetype=x86-darwin
else
    smldir=/usr/local/pkg/sml/bin/.run
    dir=.
    machinetype=x86-linux
fi

smldir=.

if test ! -r "$smldir"/run.$machinetype
then
    echo 1>&2 "Fix \$smldir"
    exit 1
fi
if test ! -r "$dir"/relplot.$machinetype
then
    echo 1>&2 "Fix \$dir"
    exit 1
fi

(
if test "$1" = "yes"
then
    cat $dir/relplot.ps
else
    sed -e 's@/uniform_scale true def@/uniform_scale false def@' $dir/relplot.ps
fi
shift

"$smldir"/run.$machinetype @SMLload="$dir/relplot.$machinetype" "$@"
echo 'showpage'
echo '%%EOF'
) | "$EPSTOPDF" --filter
