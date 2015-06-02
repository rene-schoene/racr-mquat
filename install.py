import os
try:
	from fabric.api import local, hide, quiet, task
except ImportError:
	from fabric_workaround import task, local, hide, quiet
from constants import RACR_BIN

@task(default = True)
def compile(*names):
	if names == ():
		with open('dependencies.txt') as fd:
			names = map(lambda name: name + '.scm', fd.read().splitlines())
	for name in names:
		output = 'racket-bin/mquat/{0}'.format('main_.ss' if name == 'main.scm' else (name[:-2] + 's'))
		if os.path.exists(output):
			os.remove(output)
		with hide('running'):
			local('plt-r6rs ++path {0} --install --collections racket-bin {1}'.format(RACR_BIN, name))
