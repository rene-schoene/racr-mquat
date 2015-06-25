import utils
import ilp_test as test
import ilp_measurement as measure
import ilp_check as check
import install
import sockets
from fabric.api import local, task

@task
def call_racket(f, *args):
	utils.call_racket(f, *args, capture = False)

@task
def call_larceny(f, *args):
	utils.call_larceny(f, *args, capture = False)

@task
def cloc():
	local('cloc . --exclude-dir=doc,gen,profiling,test,racket-bin,larceny-bin --not-match-f="tricks|larceny|Makefile|ls|test"')
