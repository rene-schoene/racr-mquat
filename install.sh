racrbin="/home/rschoene/git/racr/racr/racket-bin"

function compile() {
	plt-r6rs ++path $racrbin --install --collections racket-bin $1
}

#rm -rf racket-bin

cat dependencies.txt | while read line; do
	input="$line.scm"
	if [ -n "$1" ] && [[ ! "$1" == $input ]]; then continue; fi
	if [[ "$input" == "main.scm" ]]
	then output="racket-bin/mquat/main_.ss"
	else output="racket-bin/mquat/$line.ss"
	fi
	rm -f $output
	compile $input
done
