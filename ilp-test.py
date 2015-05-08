#!/usr/bin/env python

import re, unittest, os
from fabric.api import lcd, local
from fabric.colors import red

fname_sol = "test/tmp.sol"
fname_lp = "test/tmp.lp"
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


class ILPTest(unittest.TestCase):

	def run_case(self, name):
		if run_racket:
			if os.path.exists(fname_lp):
				os.remove(fname_lp)
			if os.path.exists(fname_sol):
				os.remove(fname_sol)
			# generate the ilp in test/tmp.lp
			local('racket -S %s -S %s ilp-test.scm run %s' % (RACR_BIN, MQUAT_BIN, name))
		self.assertTrue(os.path.exists(fname_lp), "ILP was not generated")
		out = local('glpsol --lp %s -o %s' % (fname_lp, fname_sol), capture = True)
		self.assertTrue(os.path.exists(fname_sol), "No solution file created")
		self.assertTrue(re.search('INTEGER OPTIMAL SOLUTION FOUND', out), "No solution found")
		return read_solution(fname_sol)

	def test_two_modes(self):
		obj,sol = self.run_case('1')
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_1#res_1'] == 1 or sol['b#comp_1##comp_1_1_1#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 10, "Wrong objective value %s" % obj)

	def test_two_modes_1max_good(self):
		obj,sol = self.run_case('2')
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_2#res_1'] == 1 or sol['b#comp_1##comp_1_1_2#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 20, "Wrong objective value %s" % obj)

	def test_two_modes_1min_good(self):
		obj,sol = self.run_case('3')
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_2#res_1'] == 1 or sol['b#comp_1##comp_1_1_2#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 30, "Wrong objective value %s" % obj)

	def test_two_modes_1req_min_good(self):
		obj,sol = self.run_case('4')
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_2#res_1'] == 1 or sol['b#comp_1##comp_1_1_2#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 40, "Wrong objective value %s" % obj)

	def test_two_modes_1req_max_good(self):
		obj,sol = self.run_case('5')
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_2#res_1'] == 1 or sol['b#comp_1##comp_1_1_2#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 50, "Wrong objective value %s" % obj)


#print read_solution('gen/printable.txt')

if __name__ == '__main__':
    unittest.main()
