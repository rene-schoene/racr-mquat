#!/usr/bin/env python

import sys, re, os, csv, timeit, shutil
from datetime import datetime
from glob import glob
try:
	from fabric.api import task, lcd
	from fabric.colors import red
except ImportError:
	from fabric_workaround import task, red, lcd
from utils import local_quiet, call_racket, call_larceny, assertTrue, assertTrueAssertion, secure_remove

assertTrue = assertTrueAssertion

class timed(object):
	def __enter__(self, msg = ' done in {0:.3f}s'):
		self.start = timeit.default_timer()
		self.msg = msg
		return self
	def __exit__(self, ex_type, value, traceback):
		self.stop = timeit.default_timer() - self.start
		print self.msg.format(self.stop)

@task(name = 'current-ids')
def current_ids():
	ids = call_racket('cli.scm', 'measure', 'dirs').replace(' ', '\n')
	print local_quiet('echo "{0}" | sort | column'.format(ids))

def setup_profiling_dirs(call_impl, cmd):
	dirs = call_impl('cli.scm', cmd, 'dirs').split()
	if not os.path.exists('profiling'):
		os.mkdir('profiling')
	for d in dirs:
		name = 'profiling/{0}'.format(d)
		if not os.path.exists(name):
			os.mkdir(name)

@task(name = 'racket-n')
def racket_n(number, *dirs):
	""" Measure Racket n times """
	do_gen(call_racket, number, dirs)

@task
def racket(*dirs):
	""" Measure Racket once """
	do_gen(call_racket, 1, dirs)

@task(name = 'racket-n')
def larceny_n(number, *dirs):
	""" Measure larceny n times """
	do_gen(call_larceny, number, dirs)

@task
def larceny(*dirs):
	""" Measure larceny once. """
	do_gen(call_larceny, 1, dirs)

def do_gen(call_impl, number, dirs):
	cmd = 'measure'
	with timed():
		setup_profiling_dirs(call_impl, cmd)
		for _ in xrange(int(number)):
			call_impl('cli.scm', cmd, 'all' if dirs == () else ' '.join(dirs), capture = False)
#			call_impl('larceny_profiling.scm', 'measure', 'all' if dirs == () else ' '.join(dirs), capture = False)
			print '\n'
#			conflate_results(skip_sol = True)

gen_results = 'gen.csv'
gen_header  = ['timestamp', 'impl', 'dir', 'step', 'ilp-gen'] # generation of ilp
gen_old_dir = '.old'

sol_results = 'sol.csv'
sol_header  = ['timestamp', 'dir', 'step', 'rows', 'cols', 'non-zero', 'ilp-sol', 'ti-ilp-sol'] # solving of ilp

att_results = 'att.csv'
att_header  = ['dir', 'flush', 'noncached', 'attname', 'executed', 'called'] # profiling attributes while generation
all_att_header  = ['dir', 'attname', 'normalex', 'normalcalled', 'flushedex', 'flushedcalled', 'noncachedex', 'noncachedcalled'] # merged profiling attributes while generation

all_results = 'all.csv'

def dirname(d):
	return os.path.split(os.path.dirname(d))[-1]

@task(name = 'sol')
def sol(number = 1, solver = 'glpsol', pathname = '*', skip_conflate = False):
	""" Run solver n times (default: once) """
	do_sol(int(number), pathname, skip_conflate)

params = { 'glpsol' : ['glpsol --tmlim 40 --lp {lp} -w {sol}', 'INTEGER OPTIMAL SOLUTION FOUND', 'Time used:[\s]*(.*?) secs', '(\d+) rows, (\d+) columns, (\d+) non-zeros'],
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
						out = local_quiet(params[solver][0].format(lp = ilp, sol = ilp.replace('lp','sol')))
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

def analyze_dir(d):
	name = dirname(d).replace('flush-', '').replace('noncached-', '')
	return [name, 1 if 'flush' in d else 0, 1 if 'noncached' in d else 0]

class att_measures_run(object):
	def __init__(self, defaultValue):
		self.computed = defaultValue
		self.called = defaultValue
	def __str__(self):
		return '{0}/{1}'.format(self.computed, self.called)
	def __iadd__(self, other):
		self.computed += other.computed
		self.called   += other.called
		return self

class att_measures_att(object):
	def __init__(self, defaultValue = 0):
		self.normal = att_measures_run(defaultValue)
		self.flushed = att_measures_run(defaultValue)
		self.noncached = att_measures_run(defaultValue)
	def __str__(self):
		return 'normal: {0}, flushed: {1}, noncached: {2}'.format(
			self.normal, self.flushed, self.noncached)
	def __iadd__(self, other):
		self.normal    += other.normal
		self.flushed   += other.flushed
		self.noncached += other.noncached
		return self
	def to_list(self):
		return [self.normal.computed, self.normal.called,
			self.flushed.computed, self.flushed.called,
			self.noncached.computed, self.noncached.called]

@task(name = 'merge-att-measurements')
def merge_att_measurements():
	runs = {}
	with open('profiling/all-att-results') as fd:
		r = csv.reader(fd)
		for row in r:
			att = None
			dir_name = row[0]
			att_name = row[3]
			runs.setdefault(dir_name, {})
			run = runs[dir_name]
			run.setdefault(att_name, att_measures_att())
			if int(row[1]) == 1:
				att = run[att_name].flushed
			elif int(row[2]) == 1:
				att = run[att_name].noncached
			else:
				att = run[att_name].normal
			att.computed = int(row[4])
			att.called = int(row[5])
	totals = {}
	with open('profiling/all-att-results.csv', 'w') as fd:
		w = csv.writer(fd)
		w.writerow(all_att_header)
		for dir_name, run in runs.iteritems():
			total = att_measures_att()
			for att_name, att in run.iteritems():
				w.writerow([dir_name, att_name] + att.to_list())
				total += att
			w.writerow([dir_name, 'total'] + total.to_list())
			totals[dir_name] = total
	with open('profiling/att-percentages.csv', 'w') as fd:
		w = csv.writer(fd)
		w.writerow(['dir', 'normalBaseline', 'flushedBaseline', 'noncachedBaseline',
			'ratioToFlushed', 'ratioToNoncached',
			'speedupToFlushed', 'speedupToNoncached'])
		baseline = lambda att: att.computed * 1.0 / att.called if att.called > 0 else 0
		ratio = lambda normal, y: normal.computed * 1.0 / y.called if y.called > 0 else 0
		for dir_name, total in totals.iteritems():
			normal, flushed, noncached = total.normal, total.flushed, total.noncached
			w.writerow([dir_name, baseline(normal), baseline(flushed), baseline(noncached),
				ratio(normal, flushed), ratio(normal, noncached),
				baseline(flushed) - ratio(normal, flushed),
				baseline(noncached) - ratio(normal, noncached)])


@task(name = 'conflate-results')
def conflate_results(pathname = '*', skip_gen = False, skip_sol = False, impls = 'larceny:plt-r6rs'):
	""" Read lp.time and gen.csv files to produce gen-X-results.csv and sol-Y-results.csv """
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
						tokens = fd.readline().split()
						impl = tokens[0].split('/')[-1]
						gen_time = '.'.join(tokens[1:3])
					row = [mod.isoformat(), impl, dirname(d), f.split('.')[0], gen_time]
					#print row
					writer.writerow(row)
					os.rename(f, os.path.join(gen_old_dir, os.path.basename(f)))

			att_measures = glob('*-att.csv')
			# Search for the one with the highest value for to-ilp, if any
			max_count = -1
			for f in att_measures:
				with open(f) as fd:
					contents = fd.readlines()
				count = next(int(line[line.rindex(',')+1:-1]) for line in contents if line.startswith('to-ilp ,'))
				print f, count, max_count
				if count > max_count:
					contents_hightest = contents
					max_count = count
#					sys.stdout.write('new highest {}\r'.format(f))
#					sys.stdout.flush()
#			sys.stdout.write('\n')
			if len(att_measures) > 0:
				with open(att_results, 'w') as fd:
					w = csv.writer(fd)
					w.writerow(att_header)
					for line in contents_hightest:
						if line.isspace():
							continue
						w.writerow(analyze_dir(d) + map(lambda e : e.strip(), line.split(',')))

			os.chdir(old_cd)

		print ' done'
		local_quiet('tail -qn +2 profiling/*/{0} > profiling/all-gen-results'.format(gen_results), capture = False)
		for impl in impls.split(':'):
			shutil.copy('profiling/gen-header', 'profiling/gen-{0}-results.csv'.format(impl))
			local_quiet('grep {0} profiling/all-gen-results | sed "s/{0},//" >> profiling/gen-{0}-results.csv'.format(impl))

		local_quiet('tail -qn +2 profiling/*/{0} >> profiling/all-att-results'.format(att_results), capture = False)
		merge_att_measurements()
	local_quiet('tail -n +1 profiling/*/specs > profiling/all-specs', capture = False)

	if not skip_sol:
		# sol-results
		local_quiet('tail -qn +2 profiling/sol-header profiling/*/{0}> profiling/sol-glpk-results.csv'.format(sol_results), capture = False)

@task(name = 'clean-att-measures')
def clean_att_measures():
	""" Remove attribute-calling profiling data """
	local_quiet('rm profiling/att-measure*.csv', capture = False)

@task(name = 'prepare-noncached')
def prepare_noncached():
	""" Set ILP Generation to noncached behavior """
	local_quiet('touch ilp-measurement*.scm')
	local_quiet('make ilp-noncached.scm', capture = False)
	secure_remove({'racket-bin/mquat': ['ilp.ss'], 'racket-bin/mquat/compiled': ['ilp_ss.dep', 'ilp_ss.zo'],
					'racket-bin/mquat/compiled/drracket/errortrace': ['ilp_ss.dep', 'ilp_ss.zo']})
	local_quiet('sed -i "s/^ilp$/ilp-noncached/" dependencies.txt', capture = False)
	local_quiet('sed -i "s/measure.non-cached = 0/measure.non-cached = 1/" scheme.properties', capture = False)
	local_quiet('make racket', capture = False)

@task(name = 'prepare-normal')
def prepare_normal():
	""" Set ILP Generation to normal behavior """
	local_quiet('touch ilp-measurement*.scm')
	secure_remove({'racket-bin/mquat': ['ilp.ss'], 'racket-bin/mquat/compiled': ['ilp_ss.dep', 'ilp_ss.zo'],
					'racket-bin/mquat/compiled/drracket/errortrace': ['ilp_ss.dep', 'ilp_ss.zo']})
	local_quiet('sed -i "s/^ilp-noncached$/ilp/" dependencies.txt', capture = False)
	local_quiet('sed -i "s/measure.non-cached = 1/measure.non-cached = 0/" scheme.properties', capture = False)
	local_quiet('make racket', capture = False)

@task
def properties():
	""" Checks dependencies.txt and scheme.properties """
	noncached = False
	with open('dependencies.txt') as fd:
		if 'ilp-noncached\n' in fd:
			noncached = True
	with open('scheme.properties') as fd:
		for line in fd:
			if line.startswith('measure.flush'):
				flushed = int(line.split('=')[1].strip()) == 1
	print 'Evaluation is {0} and {1}.'.format(
		red('non-cached') if noncached else 'cached',
		red('flushed') if flushed else 'unflushed')

@task
def help():
	print 'Measurement driver for racr-mquat'
	print '1. Do generation (racket[-n], larceny[-n]), n defaults to 1.'
	print '2. (Optional) Do solving (sol[-n], n defaults to 1.)'
	print '3. (Mandatory) Do conflate-results, to update the {gen/sol}-*-results.csv files'

if __name__ == '__main__':
	properties()
	help()
