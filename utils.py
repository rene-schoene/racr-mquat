import sys, os, csv, shutil
try:
	from fabric.api import local, quiet, task
except ImportError:
	from fabric_workaround import local, quiet
from constants import racketBin, larcenyBin, racketExec, larcenyExec

def assertTrueAssertion(expr, msg):
	if not expr:
		raise AssertionError(msg)

def assertTrueExit(expr, msg):
	if not expr:
		print msg
		sys.exit(0)

assertTrue = assertTrueExit

def local_quiet(cmd, abort_on_stderr = False, capture = True):
	""" Runs the command quietly (by default), asserts successful execution and returns stdout """
	if capture:
		with quiet():
			out = local(cmd, capture = True)
	else:
		out = local(cmd)
	assertTrue(out.succeeded and (not abort_on_stderr or len(out.stderr) == 0),
		'"{0}" not successful\nstdout:\n{1}\nstderr:\n{2}'.format(cmd, out.stdout, out.stderr))
	return out

def call_racket(f, *args, **kwargs):
	return local_quiet('{0} ++path {1} ++path {2} {3} {4}'.format(racketExec,
		racketBin.racr_bin, racketBin.mquat_bin, f, ' '.join(str(x) for x in args)),
		capture = kwargs.get('capture', True))

def call_larceny(f, *args, **kwargs):
	return local_quiet('{0} --r6rs --path {1}:{2} --program {3} -- {4}'.format(larcenyExec,
		larcenyBin.racr_bin, larcenyBin.mquat_bin, f, ' '.join(str(x) for x in args)),
		abort_on_stderr = True, capture = kwargs.get('capture', True))

def secure_remove(spec, globbing = False, dryrun = False):
	""" Attempts to remove the given files in the given directories.
	@param:spec: dict of the form { dir: [file1, file2] }
	@param:globbing: if globbing is enable (handling of *), disabled by default
	@param:dryrun: only lists files to be deleted, disabled by default """
	def remove(f):
		if os.path.exists(f):
			if dryrun:
				print 'rm "{}"'.format(f)
			else:
				os.remove(f)
	total = 0
	for d,l in spec.iteritems():
		for f in l:
			name = os.path.join(d, f)
			if globbing and '*' in f:
				from glob import glob
				for instance in glob(name):
					remove(instance)
					total += 1
			else:
				remove(name)
				total += 1
	return total

@task
def merge_csv(f1, f2, primaryColumn1 = 0, primaryColumn2 = 0, dryrun = False):
    """ Merges second csv file into first csv """
    if dryrun:
        print 'Merge {0} into {1}'.format(f2, f1)
        return
    dir1 = os.path.dirname(f1)
    if not os.path.exists(dir1):
        print 'Creating directory "{}"'.format(os.path.dirname(f1))
        os.mkdir(dir1)
    if not os.path.exists(f1):
        print 'Copying "{0}" to "{1}"'.format(f2, f1)
        shutil.copy(f2, f1)
        return
	with open(f1) as fd1, open(f2) as fd2:
		csv1, csv2 = csv.reader(fd1), csv.reader(fd2)
		result_rows = [row for row in csv1]
		primaries = set([row[primaryColumn1] for row in result_rows])
		for row in csv2:
			if row[primaryColumn2] not in primaries:
				result_rows.append(row)
	with open(f1, 'w') as fd1:
		csv1 = csv.writer(fd1)
		csv1.writerows(result_rows)

@task
def ccsv():
	with open('a.csv', 'w') as fd1, open('b.csv', 'w') as fd2:
		fd1.write('a,b,c\n')
		fd1.write('1,b1,c1\n')
		fd1.write('2,b2,c2\n')
		fd1.write('3,b3,c3\n')

		fd2.write('a,b,c\n')
		fd2.write('3,b3.1,c3.1\n')
		fd2.write('4,b4,c4\n')
		fd2.write('5,b5,c4\n')
