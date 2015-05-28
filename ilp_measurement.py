#!/usr/bin/env python

import sys, re, os, csv, timeit
from datetime import datetime
from glob import glob
from fabric.colors import red
from fabric.api import task, local, hide, quiet
from constants import RACR_BIN, MQUAT_BIN

def checked_local(cmd, abort_on_error = True):
	with quiet():
		out = local(cmd, capture = True)
	if out.failed:
		print '"%s" not successful, stdout:\n%s\nstderr:\n%s' % (cmd, out.stdout, out.stderr)
		if abort_on_error:
			sys.exit(1)
	return out

def setup_profiling_dirs():
	dirs = checked_local('racket -S %s -S %s ilp-measurement.scm dirs' % (RACR_BIN, MQUAT_BIN)).split()
	if not os.path.exists('profiling'):
		os.mkdir('profiling')
	for d in dirs:
		name = 'profiling/%s' % d
		if not os.path.exists(name):
			os.mkdir(name)

@task
def measure_racket(number = 1, *dirs):
	setup_profiling_dirs()
	for _ in xrange(int(number)):
		local('racket -S %s -S %s ilp-measurement.scm %s' % (RACR_BIN, MQUAT_BIN, 'all' if dirs == () else ' '.join(dirs)))
		conflate_results(skip_sol = True)

gen_results = 'gen.csv'
gen_header  = ['timestamp', 'dir', 'step', 'ilp-gen'] # generation of ilp
gen_old_dir = '.old'

sol_results = 'sol.csv'
sol_header  = ['timestamp', 'dir', 'step', 'rows', 'cols', 'non-zero', 'ilp-sol', 'ti-ilp-sol'] # solving of ilp

all_results = 'all.csv'

def dirname(d):
	return os.path.split(os.path.dirname(d))[-1]

@task
def measure_glpsol(pathname = '*', number = 1, skip_conflate = False):
	old_cd = os.getcwd()
	dirs = glob('profiling/%s/' % pathname)
	dirs.sort()
	for d in dirs:
		if not os.path.isdir(d):
		  print red("Not a valid directory: %s" % d)
		  continue
		sys.stdout.write(d+':')
		os.chdir(d)
		add_header = not os.path.exists(sol_results)
		with open(sol_results, 'a') as fd:
			writer = csv.writer(fd)
			if add_header:
				writer.writerow(sol_header)
			files = os.listdir('.')
			files.sort()
			for ilp in files:
				if not ilp.endswith('.lp'):
					continue
				for _ in xrange(int(number)):
					start = timeit.default_timer()
					out = checked_local('glpsol --lp %s -w %s' % (ilp, ilp.replace('lp','sol')))
					stop = timeit.default_timer()
					today = datetime.today()
					if re.search('INTEGER OPTIMAL SOLUTION FOUND', out):
						duration = re.search('Time used:[\s]*(.*?) secs', out).group(1)
						# stats=row,col,nonzero
						stats = re.search('(\d+) rows, (\d+) columns, (\d+) non-zeros', out).groups()
						sys.stdout.write('.')
					else:
						sys.stdout.write(red('!'))
						duration = -1
					sys.stdout.flush()
					row = list((today.isoformat(),dirname(d), ilp.rsplit('.', 1)[0]) + stats + (duration, stop-start))
					writer.writerow(row)
		os.chdir(old_cd)
		print ' done'
	if not skip_conflate:
		conflate_results(pathname = pathname, skip_gen = True)

@task
def conflate_results(pathname = '*', skip_gen = False, skip_sol = False):
	if not skip_gen:
		old_cd = os.getcwd()
		dirs = glob('profiling/%s/' % pathname)
		sys.stdout.write('Conflating gen-results:')
		for d in dirs:
			if not os.path.isdir(d):
			  print red("Not a valid directory: %s" % d)
			  continue
			os.chdir(d)
			sys.stdout.write('.')
			sys.stdout.flush()
			if not os.path.exists(gen_old_dir):
				os.mkdir(gen_old_dir)
			
			add_header = not os.path.exists(gen_results)
			# gen-results
			with open(gen_results, 'a') as fd:
				writer = csv.writer(fd)
				if add_header:
					writer.writerow(gen_header)

				files = glob('*.lp.time')
				files.sort()
				for f in files:
					mod = datetime.fromtimestamp(os.path.getctime(f))
					with open(f) as fd:
						gen_time = '.'.join(fd.readline().split())
					row = [mod.isoformat(),dirname(d), f.split('.')[0], gen_time]
					#print row
					writer.writerow(row)
					os.rename(f, os.path.join(gen_old_dir, os.path.basename(f)))
			os.chdir(old_cd)
		print ' done'
		local('tail -qn +2 profiling/gen-header profiling/*/%s > profiling/all-gen-results.csv' % gen_results)

	local('tail -n +1 profiling/*/specs > profiling/all-specs')

	if not skip_sol:
		# sol-results
		local('tail -qn +2 profiling/sol-header profiling/*/%s> profiling/all-sol-results.csv' % sol_results)

if __name__ == '__main__':
	measure_racket()
	measure_glpsol()
