import subprocess

class Result(object):

	def __init__(self, out, err, exit_code):
		self.stdout = out
		self.stderr = err
		self.exit_code = exit_code
		self.success = exit_code == 0
		self.failed = not self.success

	def __repr__(self):
		return self.stdout

	def __str__(self):
		return self.stdout

def local(cmd, capture = True):
	proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
	out, err = proc.communicate()
	return Result(out, err, proc.returncode)

def task(func, **kwargs):
	return func

class hide:
	def __enter__(self):
		return self
	def __exit__(self, ex_type, value, traceback):
		pass

class quiet:
	def __enter__(self):
		return self
	def __exit__(self, ex_type, value, traceback):
		pass

def red(s):
	return '***' + s + '***'
