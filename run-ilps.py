#!/usr/bin/env python
# First param is directory of lp*-files.

import sys, re, os, csv
from fabric.api import lcd, local
from fabric.colors import red

if len(sys.argv) == 1:
	print 'Usage run-ilps.py TEST_DIR'
	sys.exit(1)

d = sys.argv[1]
results = 'results.csv'
header = 'step, ilpgen, ilpsol'

if not os.path.isdir(d):
  print "No directory given: %s" % d
  sys.exit(2)

os.chdir(d)
with open(results, 'w') as fd:
	writer = csv.writer(fd)
	files = os.listdir('.')
	files.sort()
	for ilp in files:
		if not ilp.endswith('.lp'):
			continue
		out = local('glpsol --lp %s -w %s' % (ilp, ilp.replace('lp','sol')), capture = True)
		if re.search('INTEGER OPTIMAL SOLUTION FOUND', out):
			duration = re.search('Time used:[\s]*(.*?) secs', out).group(1)
			print 'Solution found for %s in %s secs' % (ilp, duration)
		else:
			print red('No solution found for %s' % ilp)
			duration = -1
		with open(ilp+'.time') as tfd:
			s,ns = tfd.readline().split()
		writer.writerow([ilp.rsplit('.', 1)[0], '.'.join([s,ns]), duration])
