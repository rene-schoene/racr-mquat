# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: R. Schöne
class Bin(object):
	def __init__(self, racr_bin, mquat_bin):
		self.racr_bin = racr_bin
		self.mquat_bin = mquat_bin

racketBin  = Bin("$HOME/git/2bt/racr/racr/racket-bin", "$HOME/git/racr-mquat/racket-bin")
larcenyBin = Bin("$HOME/git/2bt/racr/racr/larceny-bin", "$HOME/git/racr-mquat/larceny-bin")

##racketExec  = "$HOME/git/racket/racket/bin/plt-r6rs"
racketExec  = "plt-r6rs"
larcenyExec = "larceny"
