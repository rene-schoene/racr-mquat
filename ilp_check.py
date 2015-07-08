#!/usr/bin/env python

import json, codecs
try:
	from fabric.api import task
except ImportError:
	from fabric_workaround import task
from ilp_test import read_solution, _lp_file, _solution_file

@task
def get_new_sol_file(test_nr):
	""" Get name of new solution file """
	return "test/{0}-new.sol".format(test_nr)

@task
def get_new_lp_file(test_nr):
	""" Get name of new LP file """
	return "test/{0}-new.lp".format(test_nr)

@task
def new_sol(test_nr):
	""" Create new solution file """
	_, sol = read_solution(_solution_file(test_nr))
	with open(get_new_sol_file(test_nr), 'w') as fd:
		json.dump(sol, fd, indent=0)

@task
def eval(test_nr):
	""" Evaluate new solution file, i.e. print each line of LP with concrete values """
	with open(get_new_sol_file(test_nr)) as fd:
		sol = json.load(fd)
	with open(_lp_file(test_nr)) as fd:
		out = make_subs(fd.read(), sol)
	with codecs.open(get_new_lp_file(test_nr), 'w', encoding = "utf-8") as fd:
		for line in out:
			fd.write(line)
	print get_new_lp_file(test_nr)

def make_subs(text, sol):
	for key,value in sol.iteritems():
		text = text.replace("{0} ".format(key), u"\xb7{0} ".format(value)) #added a trailing space to only replace exact name matches
	return text
