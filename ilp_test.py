#!/usr/bin/env python
# -*- coding: utf-8 -*-
# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: R. Schoene

import re, threading, os, shutil, sys, timeit
try:
	from fabric.api import task
except ImportError:
	from fabric_workaround import task
import utils, properties
from utils import local_quiet, assertTrue, assertTrueAssertion

NUM_PROCESSORS = 4

utils.assertTrue = assertTrueAssertion

@task(name = 'racket')
def run_racket(*given_ranges):
	""" Run ILP tests using Racket """
	do_run(utils.call_racket, given_ranges)

@task(name = 'larceny')
def run_larceny(*given_ranges):
	""" Run ILP tests using larceny """
	do_run(utils.call_larceny, given_ranges)

def do_run(call_impl, given_ranges):
	if not os.path.exists('test'):
		os.mkdir('test')
	supported_ranges = get_ranges(call_impl)
	ranges = parse_ranges(supported_ranges, given_ranges)
	print "Ranges:", ranges
	if not ranges:
		print 'No test matches {0}. Aborting.'.format(list(given_ranges))
		sys.exit(1)

	# disable profiling as it disturbs ilp output
	properties.profiling.value = False
	properties.profiling.write_value()

	test_ids = []
	for lb,ub in ranges:
		test_ids.extend(range(lb,ub+1))
	progress = TestProgress(test_ids)
	number_tests = len(test_ids)
	threads = []
	start = timeit.default_timer()
	for i in xrange(NUM_PROCESSORS):
		t = threading.Thread(target = loop, args = (progress,i,call_impl))
		threads.append(t)
		t.start()
	for t in threads:
		t.join()
	stop = timeit.default_timer()
	print '\n'
	if progress.has_failures():
		print '=' * 30
		print 'FAILURE\n{0}'.format(progress.failure_msg)
	else:
		print 'OK'
	print '\nRan {0} tests in {1:.3f}s'.format(number_tests - len(test_ids), stop - start)

def get_ranges(call_impl):
	intervals = call_impl('cli.scm', 'test', 'ranges')
	if intervals.failed or len(intervals) == 0:
		print "Could not get intervals"
		print intervals.stdout
		print intervals.stderr
		sys.exit(1)
	result = []
	lb = False
	for token in intervals.split():
		if not lb:
			lb = int(token)
		else:
			result.append( (lb,int(token)) )
			lb = False
#	print "Ranges:", result
	return result

def intersect(a0, a1, b0, b1):
	"""
|	  Case 1		Case 2		Case 3
|	  ======		======		======
|	a0   a1		  a0   a1	a0   a1
|	 -----		   -----	 -----
|	   -----	 -----				 -----
|	  b0   b1	b0   b1				b0   b1
|	    ↓			↓			↓
|	   ---		   ---		  False
|	  b0 a1		  a0 b1
	"""
	if a0 <= b0:
		if a1 < b0:
			return False
		return (b0, a1 if a1<b1 else b1)
	else:
		return intersect(b0,b1,a0,a1)

def _notFalse(item):
	return item is not False

def _flatten(l):
	return [item for sublist in l for item in sublist]
	
def _merge_ranges(supported_ranges, given_ranges):
	# filter for empty intervals
	return filter(_notFalse,
		_flatten(map(lambda (s_min,s_max):
			map(lambda (g_min, g_max): intersect(s_min, s_max, g_min, g_max), given_ranges),
		supported_ranges)))

def parse_ranges(supported_ranges, ranges):
	if ranges == ():
		return supported_ranges
	if ranges[0].endswith("+"):
		ranges = (ranges[0][:-1], sys.maxint)
	ranges = map(int, ranges)
	if len(ranges) == 1:
		return _merge_ranges(supported_ranges, [(ranges[0], ranges[0])])
	else:
		return _merge_ranges(supported_ranges, [(ranges[i], ranges[i+1]) for i in xrange(0, len(ranges), 2)])

def read_solution(fname):
	status = 0 # 0=search for column activities, 1=skip line, 2=name, 3=values
	sol = {}
	name = ''
	obj_matcher = re.compile(r'obj = ([^\s]*)')
	combined_matcher = re.compile(r'(\d+) ([^\s]+)\s*\*?\s*(\d*\.?\d+)\s*(\d*\.?\d+)\s*(\d*\.?\d+)') #1=column,2=name,3=activity,4=lb,5=ub
	name_matcher  = re.compile(r'(\d+) ([^\s]+)') #1=column,2=name
	value_matcher = re.compile(r'\*?\s*(\d*\.?\d+)\s*(\d*\.?\d+)\s*(\d*\.?\d+)') #1=activity,2=lb,3=ub
	with open(fname) as fd:
		for line in fd:
			line = line.strip()
			#print "line = >{0}<".format(line)
			if status == 0:
				m = obj_matcher.search(line)
				if m:
					obj = float(m.group(1))
				if re.search('Column name', line):
					#print "found column line"
					status = 1
			elif status == 1:
				status = 2
			elif status == 2: #name line
				if len(line) == 0:
					break
				m = combined_matcher.match(line)
				if m:
					name  = m.group(2)
					value = m.group(3)
					sol[name] = float(value)
					#print "found combined value line for '{0}' = '{1}'".format(name, value)
					# stay in same status
				else:
					name = name_matcher.match(line).group(2)
					#print "found name line for '{0}'".format(name)
					status = 3
			elif status == 3: #value line
				value = value_matcher.match(line).group(1)
				sol[name] = float(value)
				#print "found value line for '{0}' = '{1}'".format(name, value)
				status = 2
	return (obj,sol)

def write_solution(sol, fname):
	with open(fname, 'w') as fd:
		fd.write('(\n')
		for key,value in sol.iteritems():
			fd.write('("{0}" . {1})\n'.format(key, value))
		fd.write(')\n')

class TestProgress:
	def __init__(self, test_ids):
		self.failure_msg = None
		self.available = True
		self.C = threading.Condition()
		self.test_ids = test_ids
	def next_nr(self):
		self.C.acquire()
		while not self.available:
			self.C.wait()
		try:
			nr = None if self.has_failures() else self.test_ids.pop(0)
		except IndexError:
			nr = None
		self.C.release()
		return nr
	def failure(self, test_id, msg):
		self.C.acquire()
		self.failure_msg = 'Test-Case {0} FAILED\n{1}'.format(test_id, msg)
		self.C.notifyAll()
		self.C.release()
	def has_failures(self):
		return self.failure_msg is not None

def loop(progress, thread_id, call_impl):
	working = True
	while working:
		nr = progress.next_nr()
		if not nr:
			working = False
		else:
			try:
				run_case(nr, thread_id, call_impl)
			except AssertionError as e:
				working = False
				progress.failure(nr, e.message)
#		progress.notify()
	
def _tmp_lp(thread_nr):
	return "test/tmp_{0}.lp".format(thread_nr)

def _solution_file(test_nr):
	return "test/{0}.sol".format(test_nr)

def _scheme_solution_file(test_nr):
	return "test/{0}.scsol".format(test_nr)

def _lp_file(test_nr):
	return "test/{0}.lp".format(test_nr)

def _out_file(test_nr):
	return "test/{0}.out".format(test_nr)

def run_case(test_nr, thread_id, call_impl):
	s = '{0}.'.format(test_nr)
	sys.stdout.write(s)
	sys.stdout.flush()
	## Run scheme impl to generate ILP
	fname_sol = _solution_file(test_nr)
	fname_lp_python = _lp_file(test_nr)
	fname_scheme_sol = _scheme_solution_file(test_nr)
	fname_lp_scheme = _tmp_lp(thread_id)
	if os.path.exists(fname_lp_scheme):
		os.remove(fname_lp_scheme)
	if os.path.exists(fname_sol):
		os.remove(fname_sol)
	out = call_impl('cli.scm', 'test', 'run', test_nr, fname_lp_scheme)
	with open(_out_file(test_nr), "w") as fd:
		fd.write(out)

	assertTrue(os.path.exists(fname_lp_scheme), "ILP was not generated\n{0}".format(out))
	shutil.move(fname_lp_scheme, fname_lp_python)
	
	## Solve the ILP with glpsol
	out = local_quiet('glpsol --lp {0} -o {1}'.format(fname_lp_python, fname_sol))
	assertTrue(os.path.exists(fname_sol), "No solution file created")
	obj,sol = read_solution(fname_sol)
	
	## Write the solution to fname_sc_sol
	write_solution(sol, fname_scheme_sol)

	if not re.search('INTEGER OPTIMAL SOLUTION FOUND', out):
		## No solution found
		pass
	
	## Check solution with the scheme impl
	call_impl('cli.scm', 'test', 'check', test_nr, obj, fname_scheme_sol)

if __name__ == '__main__':
	print run_racket(sys.argv[1:])
