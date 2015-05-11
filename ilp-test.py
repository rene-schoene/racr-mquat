#!/usr/bin/env python

import re, unittest, os, shutil
from fabric.api import lcd, local, task, warn_only
from fabric.colors import red

RACR_BIN="/home/rschoene/git/racr/racr/racket-bin"
MQUAT_BIN="/home/rschoene/git/racr-mquat/racket-bin"

run_racket = True

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

@task
def tws(fname):
	sol = { "b#comp_1#comp_1_2": 1, "b#comp_1#comp_1_1#comp_1_1_2#res_1": 0}
	write_solution(sol, fname)

@task
def f(cmd):
	with warn_only():
		out = local(cmd)
	print out.succeeded

def write_solution(sol, fname):
	with open(fname, 'w') as fd:
		fd.write('(\n')
		for key,value in sol.iteritems():
			fd.write('("%s" . %s)\n' % (key, value))
		fd.write(')\n')

class AbstractILPTest(unittest.TestCase):

	longMessage = True
	fname_lp_racket = "test/tmp.lp"

	def solution_file(self, test_nr):
		return "test/%s.sol" % test_nr

	def scheme_solution_file(self, test_nr):
		return "test/%s.scsol" % test_nr

	def lp_file(self, test_nr):
		return "test/%s.lp" % test_nr

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
			local('racket -S %s -S %s ilp-test.scm run %s' % (RACR_BIN, MQUAT_BIN, test_nr))
			shutil.copyfile(self.fname_lp_racket, fname_lp_python)
		self.assertTrue(os.path.exists(fname_lp_python), "ILP was not generated")
		
		## Solve the ILP with glpsol
		with warn_only():
			out = local('glpsol --lp %s -o %s' % (fname_lp_python, fname_sol), capture = True)
		if not out.succeeded:
			print out
		self.assertTrue(os.path.exists(fname_sol), "No solution file created")
		self.assertTrue(re.search('INTEGER OPTIMAL SOLUTION FOUND', out), "No solution found")
		obj,sol = read_solution(fname_sol)
		
		## Write the solution to fname_sc_sol
		write_solution(sol, fname_scheme_sol)
		
		## Check solution with Racket
		with warn_only():
			result = local('racket -S %s -S %s ilp-test.scm check %s %s %s' % (RACR_BIN, MQUAT_BIN, test_nr, obj, fname_scheme_sol))
		self.assertTrue(result.succeeded, "Could not check test #%s successfully" % test_nr)

	@classmethod
	def setUpClass(cls):
		for test_nr in cls.test_numbers:
			test_func = cls.make_test_function(cls, test_nr)
			setattr(cls, 'test_{0}'.format(test_nr), test_func)

	@staticmethod
	def make_test_function(self, test_nr):
		def test(self):
			self.run_case(test_nr)
		return test


class ILPTestForModes(AbstractILPTest):

	test_numbers = [1,2,3,4,5,6,7]

class ILPTestForImpls(AbstractILPTest):

	test_numbers = [100,101,102,103,104,105,106,107]

if __name__ == '__main__':
	ILPTestForModes.setUpClass()
	ILPTestForImpls.setUpClass()
	unittest.main(failfast=True)
