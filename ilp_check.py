#!/usr/bin/env python

from ilp_test import read_solution
try:
	from fabric.api import local, task
except ImportError:
	from fabric_workaround import local, task
import json
import codecs

@task
def get_new_sol_file(test_nr):
	return "test/{0}-new.sol".format(test_nr)

@task
def get_new_lp_file(test_nr):
	return "test/{0}-new.lp".format(test_nr)

@task
def new_sol(test_nr):
	_, sol = read_solution(ILPTest.solution_file(test_nr))
	with open(get_new_sol_file(test_nr), 'w') as fd:
		json.dump(sol, fd, indent=0)

@task
def eval(test_nr):
	with open(get_new_sol_file(test_nr)) as fd:
		sol = json.load(fd)
	with open(ILPTest.lp_file(test_nr)) as fd:
		out = make_subs(fd.read(), sol)
	with codecs.open(get_new_lp_file(test_nr), 'w', encoding = "utf-8") as fd:
		for line in out:
			fd.write(line)
	print get_new_lp_file(test_nr)

def make_subs(text, sol):
	for key,value in sol.iteritems():
		text = text.replace("{0} ".format(key), u"\xb7{0} ".format(value)) #add a trailing space to only replace exact name matches
	return text
