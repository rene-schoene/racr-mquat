#!/usr/bin/env python

import sys, re, os, csv, timeit
from datetime import datetime
from glob import glob
try:
	from fabric.colors import red
	from fabric.api import local, hide, quiet, task
except ImportError:
	from fabric_workaround import task, local, hide, quiet, red
from constants import RACR_BIN, MQUAT_BIN

class timed(object):
	def __enter__(self, msg = ' done in {1:.3f}s'):
		self.start = timeit.default_timer()
		self.msg = msg
		return self
	def __exit__(self, ex_type, value, traceback):
		self.stop = timeit.default_timer() - self.start
		print self.msg.format(self.stop)

def checked_local(cmd, abort_on_error = True):
	with quiet():
		out = local(cmd, capture = True)
	if out.failed:
		print '"{0}" not successful, stdout:\n{1}\nstderr:\n{2}'.format(cmd, out.stdout, out.stderr)
		if abort_on_error:
			sys.exit(1)
	return out

def setup_profiling_dirs():
	dirs = checked_local('plt-r6rs ++path {0} ++path {1} ilp-measurement.scm dirs'.format(RACR_BIN, MQUAT_BIN)).split()
	if not os.path.exists('profiling'):
		os.mkdir('profiling')
	for d in dirs:
		name = 'profiling/{0}'.format(d)
		if not os.path.exists(name):
			os.mkdir(name)

@task(name = 'racket-n')
def racket_n(number, *dirs):
	do_racket(number,dirs)

@task
def racket(*dirs):
	do_racket(1,dirs)

def do_racket(number, dirs):
	with timed():
		setup_profiling_dirs()
		for _ in xrange(int(number)):
			local('plt-r6rs ++path {0} ++path {1} ilp-measurement.scm {2}'.format(RACR_BIN, MQUAT_BIN, 'all' if dirs == () else ' '.join(dirs)))
			print '\n'
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
def sol(solver, pathname = '*', skip_conflate = False):
	do_sol(solver, 1, pathname, skip_conflate)

@task(name = 'glpsol-n')
def sol_n(solver, number, pathname = '*', skip_conflate = False):
	do_sol(number, pathname, skip_conflate)

params = { 'glpsol' : ['glpsol --lp {lp} -w {sol}', 'INTEGER OPTIMAL SOLUTION FOUND', 'Time used:[\s]*(.*?) secs', '(\d+) rows, (\d+) columns, (\d+) non-zeros'],
		   'gurobi' : ['gurobi_cl ResultFile={sol} {lp}', 'Optimal solution found', 'in (.*?) seconds', 'Optimize a model with (\d+) rows, (\d+) columns and (\d+) nonzeros']}

def do_sol(solver, number, pathname, skip_conflate):
	old_cd = os.getcwd()
	dirs = glob('profiling/{0}/'.format(pathname))
	dirs.sort()
	for d in dirs:
		if not os.path.isdir(d):
			print red("Not a valid directory: {0}".format(d))
			continue
		with timed():
			total_start = timeit.default_timer()
			sys.stdout.write(d)
			os.chdir(d)
			add_header = not os.path.exists(sol_results)
			with open(sol_results, 'a') as fd:
				writer = csv.writer(fd)
				if add_header:
					writer.writerow(sol_header)
				files = os.listdir('.')
				files.sort()
				for _ in xrange(int(number)):
					sys.stdout.write(':')
					for ilp in files:
						if not ilp.endswith('.lp'):
							continue
						start = timeit.default_timer()
						out = checked_local(params[solver][0].format(lp = ilp, sol = ilp.replace('lp','sol')))
						stop = timeit.default_timer()
						today = datetime.today()
						if re.search(params[solver][1], out):
							duration = re.search(params[solver][2], out).group(1)
							# stats=row,col,nonzero
							stats = re.search(params[solver][3], out).groups()
							sys.stdout.write('.')
						else:
							sys.stdout.write(red('!'))
							duration = -1
						sys.stdout.flush()
						row = list((today.isoformat(),dirname(d), ilp.rsplit('.', 1)[0]) +
									stats + (duration, stop-start))
						writer.writerow(row)
			os.chdir(old_cd)
	if not skip_conflate:
		conflate_results(pathname = pathname, skip_gen = True)

@task(name = 'conflate-results')
def conflate_results(pathname = '*', skip_gen = False, skip_sol = False):
	if not skip_gen:
		old_cd = os.getcwd()
		dirs = glob('profiling/{0}/'.format(pathname))
		sys.stdout.write('Conflating gen-results:')
		for d in dirs:
			if not os.path.isdir(d):
			  print red("Not a valid directory: {0}".format(d))
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
		local('tail -qn +2 profiling/gen-header profiling/*/{0} > profiling/all-gen-results.csv'.format(gen_results))

	local('tail -n +1 profiling/*/specs > profiling/all-specs')

	if not skip_sol:
		# sol-results
		local('tail -qn +2 profiling/sol-header profiling/*/{0}> profiling/all-sol-results.csv'.format(sol_results))

if __name__ == '__main__':
	racket()
	glpsol()
