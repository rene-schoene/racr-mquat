import os, subprocess

env = { 'local_wd' : None }

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

def _add_wd(cmd):
	d = env['local_wd']
	return cmd if d is None else 'cd {0} && {1}'.format(d, cmd)

def local(cmd, **kwargs):
	proc = subprocess.Popen(_add_wd(cmd), stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell = True)
	out, err = proc.communicate()
	return Result(out, err, proc.returncode)

class task(object)
	def __init__(self, f, default = False, name = None):
		self.f = f
	def __call__(self, *args, **kwargs):
		f(*args, **kwargs)

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

class lcd:
	def __enter__(self, d):
		self.former_wd = env['local_wd']
		env['local_wd'] = d if self.former_wd is None else os.path.join(self.former_wd, d)
		return self
	def __exit__(self, ex_type, value, traceback):
		env['local_wd'] = self.former_wd

def red(s):
	return '***' + s + '***'
