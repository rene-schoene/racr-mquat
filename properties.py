# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: R. Schoene
from utils import local_quiet
try:
    from fabric.api import task
    from fabric.colors import red, green
except ImportError:
    from fabric_workaround import task, red, green

properties_fname = 'scheme.properties'

def int_to_bool(i):
    return i in ('1', 1, True)

def bool_to_int(b):
    return 1 if b else 0

class PItem(object):
    def __init__(self, name, question, default, key, conv_read = int_to_bool, conv_write = bool_to_int):
        self.name = name
        self.question = question
        self.default = default
        self.key = key
        self.conv_r = conv_read
        self.conv_w = conv_write
        self._value = default
    @property
    def value(self):
        return self._value
    @value.setter
    def value(self, new_value):
        self._value = self.conv_r(new_value)
    def write_value(self):
        local_quiet(r'sed -i "s/{0}\(\s*\)= {1}/{0}\1= {2}/" {3}'.format(
            self.key, self.conv_w(not self.value), self.conv_w(self.value), properties_fname))

timing    = PItem('timing', 'Measure runtimes?', False, 'timing')
log_info  = PItem('info', 'Log INFO messages?', True, 'log.info')
log_debug = PItem('debug', 'Log Debug messages?', False, 'log.debug')
lp_write  = PItem('lp', 'Write out ILP files?', False, 'measure.lp.write')
profiling = PItem('profiling', 'Profile attribute metrics?', True, 'measure.profiling')
flushed   = PItem('flushed', 'Use strategy "flushed"?', False, 'measure.flush')
noncached = PItem('noncached', 'Use strategy "noncached"?', False, 'measure.non-cached')
preesleep  = PItem('presleep', 'Seconds to wait before running complete run?', 0.0, 'measure.presleep',
                  conv_read = float, conv_write = lambda x : x if x else '.*' )

items = [timing, log_info, log_debug, lp_write, profiling, flushed, noncached, preesleep]

#class Properties(object):
#    def __init__(self, f):
with open(properties_fname) as fd:
    for line in fd:
        if line.isspace() or line.startswith('#'):
            continue
        key, value = map(lambda x:x.strip(), line.split('='))
        item = next((e for e in items if e.key == key), None)
        if item is None:
            print 'Could not find property for {}'.format(key)
        else:
            item.value = value

def confirm(question, default_val = False):
    prompt = question
    if isinstance(default_val, bool):
        prompt += ' [{0}]'.format('Y/n' if default_val else 'y/N')
    elif isinstance(default_val,int):
        prompt += ' [{0}]'.format(default_val)
    answer = raw_input(prompt + ' ')
    if answer == '':
        answer = default_val
    if isinstance(default_val, bool):
        return answer in ('y','Y','yes','Yes',True)
    elif isinstance(default_val,int):
        return int(answer)
    return answer

@task
def check():
    """ Checks dependencies.txt and scheme.properties """
    def nc_tostring(val):
        return red('non-cached') if val else 'cached'
    noncached_scm = False
    with open('dependencies.txt') as fd:
        if 'ilp-noncached\n' in fd:
            noncached_scm = True
    print '\n- '.join(( 'Evaluation is set to:',
        red('non-cached') if noncached.value else 'cached',
        red('flushed') if flushed.value else 'unflushed',
        (green('Yes: ') if timing.value else 'No ') + 'measurement of execution times',
        (green('Yes: ') if profiling.value else 'No ') + 'profiling of attribute metrics',
        (green('Yes: ') if lp_write.value else 'No ') + 'write of LP files',
        'Wait {0} second(s) before each experiment'.format(preesleep.value)))
    if noncached_scm != noncached.value:
        print 'Attention: Compiled ilp ({}) differs from properties file setting ({}).'.format(
            nc_tostring(noncached_scm), nc_tostring(noncached.value))
    if timing.value and profiling.value:
        print 'Attention: Both, enabled profiling will influence timing.'
    if flushed.value and noncached.value:
        print 'Disabling "flushed", as noncached is enabled'
        flushed.value = False
    if not (timing.value or lp_write.value or profiling.value):
        print 'Nothing is done or measured, either set timing, lp_write or profiling'

@task
def setup(name = None, to_default = False):
    """
    Interactive setup of all settings or given specific one.
    Overrides the file "scheme.properties"
    """
    def default_val(item):
        return item.default if to_default else item.value
    def confirm_s(wanted, item):
        if not wanted or item.name.startswith(wanted):
            return confirm(item.question, default_val(item))
        return default_val(item)
    for p in items:
        p.value = confirm_s(name, p)

    # consistency checking
    check()

    for p in items:
        p.write_value()
    print 'Remember to invoke prepare-{} if noncached setting was changed'.format(
        'noncached' if noncached.value else 'normal')
