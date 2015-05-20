#!/usr/bin/env python

import sys, re, os, csv
from fabric.colors import red
from fabric.api import task, local, lcd
from constants import RACR_BIN, MQUAT_BIN

@task
def measure_racket():
	dirs = local('racket -S %s -S %s ilp-measurement.scm dirs' % (RACR_BIN, MQUAT_BIN), capture = True).split()
	if not os.path.exists('profiling'):
		os.mkdir('profiling')
	for d in dirs:
		name = 'profiling/%s' % d
		if not os.path.exists(name):
			os.mkdir(name)
	local('racket -S %s -S %s ilp-measurement.scm all' % (RACR_BIN, MQUAT_BIN))

results = 'results.csv'
header = 'step, ilpgen, ilpsol'

@task
def measure_glpsol(*dirs):
	old_cd = os.getcwd()
	if dirs == ():
		from glob import glob
		dirs = glob('profiling/*/')
		
	for d in dirs:
		os.chdir(old_cd)
		if not os.path.isdir(d):
		  print red("Not a valid directory: %s" % d)
		  continue
		print d
		os.chdir(d)
		with open(results, 'w') as fd:
			writer = csv.writer(fd)
			files = os.listdir('.')
			files.sort()
			for ilp in files:
				if not ilp.endswith('.lp'):
					continue
				out = local('glpsol --lp %s -w %s' % (ilp, ilp.replace('lp','sol')), capture = True)
				if re.search('INTEGER OPTIMAL SOLUTION FOUND', out):
					duration = re.search('Time used:[\s]*(.*?) secs', out).group(1)
					print 'Solution found for %s in %s secs' % (ilp, duration)
				else:
					print red('No solution found for %s' % ilp)
					duration = -1
				with open(ilp+'.time') as tfd:
					s,ns = tfd.readline().split()
				writer.writerow([ilp.rsplit('.', 1)[0], '.'.join([s,ns]), duration])

@task
def t(*dirs):
	print dirs
	if dirs == ():
		from glob import glob
		dirs = glob('profiling/*/')
	print dirs

if __name__ == '__main__':
	measure()
