# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# Author: R. Schöne
from utils import local_quiet

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
