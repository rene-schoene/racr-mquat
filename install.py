# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: R. Schoene
import os, shutil, sys
try:
	from fabric.api import lcd, task
except ImportError:
	from fabric_workaround import lcd, task
from constants import racketBin, larcenyBin, racketExec, larcenyExec
from utils import local_quiet

def parse(names = None):
	if not names:
		with open('dependencies.txt') as fd:
			deps = fd.read().splitlines()
		deps = filter(lambda x : not x.startswith('#'), deps[deps.index('@sources:')+1:])
		return map(lambda name: name + '.scm', deps)
	return names

@task(name = 'all', default = True)
def compile_all():
	""" Compile each dependency for both, Racket and larceny. """
	compile_racket()
	compile_larceny()

@task(name = 'racket')
def compile_racket(*names):
	""" Compile every listed source for Racket.
	If none given, compile each dependency.	"""
	for name in parse(names):
		output = 'racket-bin/mquat/{0}'.format('main_.ss' if name == 'main.scm' else (name[:-2] + 's'))
		if os.path.exists(output):
			os.remove(output)
		local_quiet('{0} ++path {1} --install --collections racket-bin {2}'.
			format(racketExec, racketBin.racr_bin, name),
			capture = False)

compile_stale_text = """#!r6rs
(import (rnrs) (larceny compiler))
(compiler-switches (quote fast-safe))
(compile-stale-libraries)
"""

@task(name = 'larceny')
def compile_larceny():
	""" Compile each dependency for larceny """
	larceny_dir = 'larceny-bin/mquat'
	compile_stale = 'compile-stale'
	if not os.path.exists(larceny_dir):
		os.makedirs(larceny_dir)
	with open(os.path.join(larceny_dir, compile_stale), 'w') as fd:
		fd.write(compile_stale_text)
	for name in parse():
		shutil.copy2(name, os.path.join(larceny_dir, os.path.splitext(name)[0] + '.sls'))
	with lcd(larceny_dir):
		local_quiet('{0} --r6rs --path ..:{1} --program {2}'.
			format(larcenyExec, larcenyBin.racr_bin, compile_stale),
			capture = False)
