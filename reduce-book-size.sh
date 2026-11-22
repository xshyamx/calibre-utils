#!/bin/sh

SRC_DIR=./out
WORK_DIR=./work

if [ ! -d $SRC_DIR ]; then
    echo "$SRC_DIR does not exist"
    exit 1
fi

size=$(du -sh $SRC_DIR | cut -f 1)
printf "Before: %s\n" $size

mkdir -p $WORK_DIR

SIZE_THRESHOLD=10M
RESIZE_THRESHOLD=768
# try to optimize files > SIZE_THRESHOLD
echo "Optimizing books with filesize > ${SIZE_THRESHOLD} by resizing images to be atmost ${RESIZE_THRESHOLD}px"
find $SRC_DIR -type f -size +${SIZE_THRESHOLD} > $WORK_DIR/list

function reduce_book() {
    src_file="$1"
    bn=$(basename $src_file .epub)
    printf "%s..." $bn
    mkdir -p $WORK_DIR/zip
    unzip -d $WORK_DIR/zip "$src_file" >$WORK_DIR/extracted 2>&1
    img_file=$(awk '/\.(png|jpe?g|gif)/ {print $2}' $WORK_DIR/extracted | head -1)
    img_dir=$(dirname $img_file)
    if [ "$img_dir" != "" -a -d $img_dir ]; then
	pushd $img_dir >/dev/null
	find . -name '*.jpg' -o -name '*.jpeg' -o -name '*.gif' -o -name '*.png' > list
	i=0
	n=$(wc -l list | awk '{print $1}')
	d=$(expr $n / 5)
	while read f ; do
	    i=$(expr $i + 1)
	    r=$(expr $i % $d)
	    if [ $r -eq 0 ]; then
		printf "."
	    fi
	    magick $f -resize "${RESIZE_THRESHOLD}x${RESIZE_THRESHOLD}>" $f 2>/dev/null
	done < list
	popd >/dev/null
    fi
    pushd $WORK_DIR/zip >/dev/null
    if [ -f ../${bn}-reduced.epub ]; then
	rm -f ../${bn}-reduced.epub
    fi
    zip -r ../${bn}-reduced.epub * >/dev/null 2>&1
    popd >/dev/null
    oz=$(du -s $src_file | cut -f 1)
    nz=$(du -s $WORK_DIR/$bn-reduced.epub | cut -f 1)
    if [ $nz -lt $oz ]; then
	mv $WORK_DIR/$bn-reduced.epub $src_file
	echo ok
    else
	echo "failed $nz > $oz"
    fi
    rm -fr $WORK_DIR/zip $WORK_DIR/extracted
}

nb=$(wc -l $WORK_DIR/list | awk '{print $1}')
ni=0
while read file; do
    ni=$(expr $ni + 1)
    printf "[%d/%d] " $ni $nb
    reduce_book "$file"
done < $WORK_DIR/list
size=$(du -sh $SRC_DIR | cut -f 1)
printf "After: %s\n" $size

#reduce_book $(tail -n 2 $WORK_DIR/list | head -1)
