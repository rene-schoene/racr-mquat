import sys, os
try:
	from fabric.api import local, quiet
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
