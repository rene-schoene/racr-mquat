import sys
try:
	from fabric.api import local, quiet
except ImportError:
	from fabric_workaround import local, quiet
from constants import racketBin, larcenyBin

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
	return local_quiet('plt-r6rs ++path {0} ++path {1} {2} {3}'.format(
		racketBin.racr_bin, racketBin.mquat_bin, f, ' '.join(str(x) for x in args)),
		capture = kwargs.get('capture', True))

def call_larceny(f, *args, **kwargs):
#	args += ('--verbose',)
	return local_quiet('larceny --r6rs --path {0}:{1} --program {2} -- {3}'.format(
		larcenyBin.racr_bin, larcenyBin.mquat_bin, f, ' '.join(str(x) for x in args)),
		abort_on_stderr = True, capture = kwargs.get('capture', True))
