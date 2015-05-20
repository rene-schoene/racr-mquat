#!/usr/bin/env python

import sys, re, os, csv, timeit
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
header = ['step', 'ilp-gen', 'rows', 'cols', 'non-zero', 'ilp-sol', 't-ilp-sol']

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
			writer.writerow(header)
			files = os.listdir('.')
			files.sort()
			for ilp in files:
				if not ilp.endswith('.lp'):
					continue
				start = timeit.default_timer()
				out = local('glpsol --lp %s -w %s' % (ilp, ilp.replace('lp','sol')), capture = True)
				stop = timeit.default_timer()
				if re.search('INTEGER OPTIMAL SOLUTION FOUND', out):
					duration = re.search('Time used:[\s]*(.*?) secs', out).group(1)
					rows, cols, nonzeros = re.search('(\d+) rows, (\d+) columns, (\d+) non-zeros', out).groups()
					print 'Solution found for %s in %s secs (timeit: %s secs)' % (ilp, duration, stop - start)
				else:
					print red('No solution found for %s' % ilp)
					duration = -1
				with open(ilp+'.time') as tfd:
					ilp_gen_time = '.'.join(tfd.readline().split())
				writer.writerow([ilp.rsplit('.', 1)[0], ilp_gen_time, rows, cols, nonzeros, duration, stop-start])
	os.chdir(old_cd)
	conflate_results()

@task
def conflate_results():
	local('tail -n +1 profiling/*/results.csv > profiling/all-results')

@task
def t(*dirs):
	print dirs
	if dirs == ():
		from glob import glob
		dirs = glob('profiling/*/')
	print dirs

if __name__ == '__main__':
	measure()
