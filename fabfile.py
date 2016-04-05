# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: R. Schoene
import properties, utils, install, sockets
import ilp_test as test
import ilp_measurement as measure
import ilp_check as check
import os
from fabric.api import local, task, get, hosts, run

@task
def call_racket(f, *args):
	""" Invoke racket with given program and args """
	utils.call_racket(f, *args, capture = False)

@task
def call_larceny(f, *args):
	""" Invoke larceny with given program and args """
	utils.call_larceny(f, *args, capture = False)

@task(name = 'cloc-core')
def cloc_core(*args, **kwargs):
	""" Run cloc for the core """
	cloc('ast-generation|test|measure|check', *args, **kwargs)

@task(name = 'cloc-tm')
def cloc_tm(*args, **kwargs):
	""" Run cloc including testing and measurements """
	cloc('', *args, **kwargs)

def cloc(not_match, *args, **kwargs):
	not_match = 'sockets|java|tricks|example-ast|larceny|Makefile|ls' + ('|'+not_match if not_match else '')
	local('cloc . --exclude-dir=doc,gen,profiling,test,racket-bin,larceny-bin --not-match-f="{0}" {1} {2}'.format(not_match, ' '.join(args),
		' '.join("{!s}={!r}".format(k,v) for (k,v) in kwargs.iteritems())))

@task
@hosts('rschoene@141.76.65.177')
def dl(name, rdir = '~/git/racr-mquat/'):
	""" Download a single file from the remote host.
	Arguments: name, [absolute-remote-directory] """
	#run('uname -a')
	target = os.path.join(rdir, name)
	print target
	get(target, local_path = name)
