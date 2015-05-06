racrbin="/home/rschoene/git/racr/racr/racket-bin"

function compile() {
	plt-r6rs ++path $racrbin --install --collections racket-bin $1
}

rm -rf racket-bin

cat dependencies.txt | while read line; do
	compile "$line.scm"
done
