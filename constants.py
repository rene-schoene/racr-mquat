# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: R. Schoene

import os

RACR_HOME = "$HOME/git/d-2bt/racr/racr/"
PROGRAM_HOME = "$HOME/git/racr-mquat/"
PROGRAM_PACKAGE = "mquat"

class Bin(object):
	def __init__(self, racr_bin, program_bin):
		self.racr_bin = racr_bin
		self.program_bin = program_bin

racketBin  = Bin(os.path.join(RACR_HOME, "racket-bin"), os.path.join(PROGRAM_HOME, "racket-bin"))
larcenyBin = Bin(os.path.join(RACR_HOME, "larceny-bin"), os.path.join(PROGRAM_HOME, "larceny-bin"))

##racketExec  = "$HOME/git/racket/racket/bin/plt-r6rs"
racketExec  = "plt-r6rs"
larcenyExec = "larceny"
