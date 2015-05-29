#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re, unittest, os, shutil, sys
try:
	from fabric.api import local, quiet, task
	from fabric.colors import red
except ImportError:
	from fabric_workaround import local, quiet, red, task
from constants import RACR_BIN, MQUAT_BIN

run_racket = True

@task
def run(*given_ranges):
	supported_ranges = get_ranges()
	ranges = parse_ranges(supported_ranges, given_ranges)
#	print ranges
	if not ranges:
		print 'No test matches %s. Aborting.' % list(given_ranges)
		sys.exit(1)
	for lb,ub in ranges:
		ILPTest.create_ts(range(lb,ub+1))
	suite = unittest.TestLoader().loadTestsFromTestCase(ILPTest)
	unittest.TextTestRunner(verbosity=2, failfast=True).run(suite)

def get_ranges():
	with quiet():
		intervals = local('racket -S %s -S %s ilp-test.scm ranges' % (RACR_BIN, MQUAT_BIN), capture=True)
	if intervals.failed:
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
	print "Ranges:", result
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

def notFalse(item):
	return item is not False

def flatten(l):
	return [item for sublist in l for item in sublist]
	
def merge_ranges(supported_ranges, given_ranges):
	# filter for empty intervals
	return filter(notFalse,
		flatten(map(lambda (s_min,s_max):
			map(lambda (g_min, g_max): intersect(s_min, s_max, g_min, g_max), given_ranges),
		supported_ranges)))

def parse_ranges(supported_ranges, ranges):
	if ranges == ():
		return supported_ranges
	if ranges[0].endswith("+"):
		ranges = (ranges[0][:-1], sys.maxint)
	ranges = map(int, ranges)
	if len(ranges) == 1:
		return merge_ranges(supported_ranges, [(ranges[0], ranges[0])])
	else:
		return merge_ranges(supported_ranges, [(ranges[i], ranges[i+1]) for i in xrange(0, len(ranges), 2)])

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
			#print "line = >%s<" % line
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
					#print "found combined value line for '%s' = '%s'" % (name, value)
					# stay in same status
				else:
					name = name_matcher.match(line).group(2)
					#print "found name line for '%s'" % name
					status = 3
			elif status == 3: #value line
				value = value_matcher.match(line).group(1)
				sol[name] = float(value)
				#print "found value line for '%s' = '%s'" % (name, value)
				status = 2
	return (obj,sol)

def write_solution(sol, fname):
	with open(fname, 'w') as fd:
		fd.write('(\n')
		for key,value in sol.iteritems():
			fd.write('("%s" . %s)\n' % (key, value))
		fd.write(')\n')

class ILPTest(unittest.TestCase):

	longMessage = False
	fname_lp_racket = "test/tmp.lp"

	@staticmethod
	def solution_file(test_nr):
		return "test/%s.sol" % test_nr

	@staticmethod
	def scheme_solution_file(test_nr):
		return "test/%s.scsol" % test_nr

	@staticmethod
	def lp_file(test_nr):
		return "test/%s.lp" % test_nr

	def local_quiet(self, cmd):
		""" Runs the command quietly, asserts successfull execution and returns stdout """
		with quiet():
			out = local(cmd, capture = True)
			self.assertTrue(out.succeeded, '"%s" not successful, stdout:\n%s\nstderr:\n%s' % (cmd, out.stdout, out.stderr))
			return out

	def run_case(self, test_nr):
		## Run Racket to generate ILP
		fname_sol = self.solution_file(test_nr)
		fname_lp_python = self.lp_file(test_nr)
		fname_scheme_sol = self.scheme_solution_file(test_nr)
		if run_racket:
			if os.path.exists(self.fname_lp_racket):
				os.remove(self.fname_lp_racket)
			if os.path.exists(fname_sol):
				os.remove(fname_sol)
			out = self.local_quiet('racket -S %s -S %s ilp-test.scm run %s' % (RACR_BIN, MQUAT_BIN, test_nr))
			self.assertTrue(os.path.exists(self.fname_lp_racket), "ILP was not generated\n%s" % out)
			shutil.copyfile(self.fname_lp_racket, fname_lp_python)
		
		## Solve the ILP with glpsol
		out = self.local_quiet('glpsol --lp %s -o %s' % (fname_lp_python, fname_sol))
		self.assertTrue(os.path.exists(fname_sol), "No solution file created")
		obj,sol = read_solution(fname_sol)
		
		## Write the solution to fname_sc_sol
		write_solution(sol, fname_scheme_sol)

		if not re.search('INTEGER OPTIMAL SOLUTION FOUND', out):
			## No solution found
			pass
		
		## Check solution with Racket
		self.local_quiet('racket -S %s -S %s ilp-test.scm check %s %s %s' % (RACR_BIN, MQUAT_BIN, test_nr, obj, fname_scheme_sol))

	@classmethod
	def create_ts(cls, test_numbers):
		for test_nr in test_numbers:
			test_func = cls.make_t_function(cls, test_nr)
			setattr(cls, 'test_{0:03d}'.format(test_nr), test_func)

	@staticmethod
	def make_t_function(self, test_nr):
		def test(self):
			self.run_case(test_nr)
		return test

if __name__ == '__main__':
	print run(sys.argv[1:])
