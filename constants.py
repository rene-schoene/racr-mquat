class Bin(object):
	def __init__(self, racr_bin, mquat_bin):
		self.racr_bin = racr_bin
		self.mquat_bin = mquat_bin

racketBin  = Bin("$HOME/git/racr/racr/racket-bin", "$HOME/git/racr-mquat/racket-bin")
larcenyBin = Bin("$HOME/git/racr/racr/larceny-bin", "$HOME/git/racr-mquat/larceny-bin")

racketExec  = "$HOME/git/racket/racket/bin/plt-r6rs"
larcenyExec = "larceny"
