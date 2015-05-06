#!/usr/bin/env python

import sys, csv
from fabric.api import local

def schemeCall(code, dependencies=[]):
	print 'with %s execute "%s"' % (dependencies, code)
	pass #TODO

def runGlpsol(name):
	result = local('glpsol --lp -w plain_%(name)s.txt', capture = True)

with open('ilp-measurement-cases.txt') as fd:
	reader = csv.reader(fd, delimiter=' ')
	name = '' # store the name of the current case
	schemeCall(code='(define ast #f)')
	for row in reader:
		print row
		if len(row) == 0 or len(row[0].strip()) == 0 or row[0].startswith('#'):
			continue
		if row[0] == 'CASE':
			# rest: name num-pe num-pe-subs num-comp impl-per-comp mode-per-impl
			name = row[1]
			schemeCall(dependencies=['ast-generation', 'ilp'], code=
			('(set! ast (create-example-ast %(num-pe)s %(num-pe-subs)s %(num-comp)s'+
			' %(impl-per-comp)s %(mode-per-impl)s)) (save-ilp "test/%(name)s.txt" ast') %
			 {'name':name, 'num-pe':row[2], 'num-pe-subs':row[3], 'num-comp':row[4], 'impl-per-comp':row[5], 'mode-per-impl':row[6]})
		elif row[0] == 'REWRITE':
			# rest: n followed by n-times (comp-name prop-name new-value)
			n = int(row[1])
			for i in xrange(n):
				schemeCall(code=
				("(rewrite-terminal 'value"+
				" (att-value 'provided-clause (find (lambda (pe)"+
				"  (eq? (ast-child 'name pe) '%(comp-name)s))"+
				"  (att-value 'every-pe ast)) '%(prop-name)s 'theType)"+
				" (lambda _ %(new-value)s))") %
				{'comp-name': row[i*3+2], 'prop-name': row[i*3+3], 'new-value': row[i*3+4]})
