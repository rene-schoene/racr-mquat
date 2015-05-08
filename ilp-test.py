#!/usr/bin/env python

import re, unittest, os, shutil
from fabric.api import lcd, local
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


class AbstractILPTest(unittest.TestCase):

	longMessage = True
	fname_lp_racket = "test/tmp.lp"

	def solution_file(self, name):
		return "test/%s.sol" % name

	def lp_file(self, name):
		return "test/%s.lp" % name

	def run_case(self, name):
		fname_sol = self.solution_file(name)
		fname_lp_python = self.lp_file(name)
		if run_racket:
			if os.path.exists(self.fname_lp_racket):
				os.remove(self.fname_lp_racket)
			if os.path.exists(fname_sol):
				os.remove(fname_sol)
			# generate the ilp
			local('racket -S %s -S %s ilp-test.scm run %s' % (RACR_BIN, MQUAT_BIN, name))
			shutil.copyfile(self.fname_lp_racket, fname_lp_python)
		self.assertTrue(os.path.exists(fname_lp_python), "ILP was not generated")
		out = local('glpsol --lp %s -o %s' % (fname_lp_python, fname_sol), capture = True)
		self.assertTrue(os.path.exists(fname_sol), "No solution file created")
		self.assertTrue(re.search('INTEGER OPTIMAL SOLUTION FOUND', out), "No solution found")
		return read_solution(fname_sol)

class ILPTestForModes(AbstractILPTest):

	def test_2m(self):
		obj,sol = self.run_case(1)
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_1#res_1'] == 1 or sol['b#comp_1##comp_1_1_1#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 10.001, "Wrong objective value %s" % obj)

	def test_2m_1max(self):
		obj,sol = self.run_case(2)
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_2#res_1'] == 1 or sol['b#comp_1##comp_1_1_2#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 20.002, "Wrong objective value %s" % obj)

	def test_2m_1min(self):
		obj,sol = self.run_case(3)
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_2#res_1'] == 1 or sol['b#comp_1##comp_1_1_2#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 20.003, "Wrong objective value %s" % obj)

	def test_2m__req_1min(self):
		obj,sol = self.run_case(4)
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_2#res_1'] == 1 or sol['b#comp_1##comp_1_1_2#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 20.004, "Wrong objective value %s" % obj)

	def test_2m_req_1max(self):
		obj,sol = self.run_case(5)
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertTrue(sol['b#comp_1##comp_1_1_2#res_1'] == 1 or sol['b#comp_1##comp_1_1_2#res_2'] == 1,
			"First mode is not deployed")
		self.assertEqual(obj, 20.005, "Wrong objective value %s" % obj)

	def test_2m_res1(self):
		obj,sol = self.run_case(6)
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertEqual(sol['b#comp_1##comp_1_1_1#res_1'], 1, "First mode is not deployed on first resource")
		self.assertEqual(obj, 10.006, "Wrong objective value %s" % obj)

	def test_2m_res2(self):
		obj,sol = self.run_case(7)
		self.assertEqual(sol['b#comp_1#'], 1, "First implementation is not deployed!")
		self.assertEqual(sol['b#comp_1##comp_1_1_1#res_2'], 1, "First mode is not deployed on second resource")
		self.assertEqual(obj, 10.007, "Wrong objective value %s" % obj)

class ILPTestForImpls(AbstractILPTest):

	def test_2i2(self):
		obj,sol = self.run_case(100)
		self.assertEqual(sol['b#comp_1#comp_1_1'], 1, "First implementation is not deployed!")
		self.assertEqual(sol['b#comp_1#comp_1_2'], 0, "Seond implementation is deployed!")
		self.assertTrue(sol['b#comp_1#comp_1_1#comp_1_1_1#res_1'] == 1 or sol['b#comp_1#comp_1_1#comp_1_1_1#res_2'] == 1
			 or sol['b#comp_1#comp_1_1#comp_1_1_1#res_3'] == 1, "First mode of first impl is not deployed")
		self.assertEqual(obj, 10.100, "Wrong objective value %s" % obj)

	def test_2i2_2nd(self):
		for test_id in [101, 105]:
			obj,sol = self.run_case(test_id)
			self.assertEqual(sol['b#comp_1#comp_1_1'], 1, "First implementation is not deployed!")
			self.assertEqual(sol['b#comp_1#comp_1_2'], 0, "Seond implementation is deployed!")
			self.assertTrue(sol['b#comp_1#comp_1_1#comp_1_1_2#res_1'] == 1 or sol['b#comp_1#comp_1_1#comp_1_1_2#res_2'] == 1
				 or sol['b#comp_1#comp_1_1#comp_1_1_2#res_3'] == 1, "Second mode of first impl is not deployed")
			self.assertEqual(obj, 15 + (test_id / 1000.0), "Wrong objective value %s" % obj)

	def test_2i2_3rd(self):
		for test_id in [102, 103, 104]:
			obj,sol = self.run_case(test_id)
			self.assertEqual(sol['b#comp_1#comp_1_1'], 0, "First implementation is deployed!")
			self.assertEqual(sol['b#comp_1#comp_1_2'], 1, "Seond implementation is not deployed!")
			self.assertTrue(sol['b#comp_1#comp_1_2#comp_1_2_1#res_1'] == 1 or sol['b#comp_1#comp_1_2#comp_1_2_1#res_2'] == 1
				 or sol['b#comp_1#comp_1_2#comp_1_2_1#res_3'] == 1, "Third mode of first impl is not deployed")
			self.assertEqual(obj, 20 + (test_id / 1000.0), "Wrong objective value %s" % obj)


#print read_solution('test/tmp.sol'); exit(0)

if __name__ == '__main__':
    unittest.main()
