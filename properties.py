from utils import local_quiet

properties_fname = 'scheme.properties'

def int_to_bool(i):
    return i in ('1', 1, True)

class PItem(object):
    def __init__(self, name, question, default, key, conv = int_to_bool):
        self.name = name
        self.question = question
        self.default = default
        self.key = key
        self.conv = conv
        self._value = default
    @property
    def value(self):
        return self._value
    @value.setter
    def value(self, new_value):
        self._value = self.conv(new_value)
    def write_value(self):
        def v(value):
            return 1 if value else 0
        local_quiet(r'sed -i "s/{0}\(\s*\)= {1}/{0}\1= {2}/" {3}'.format(
            self.key, v(not self.value), v(self.value), properties_fname))

timing    = PItem('timing', 'Measure runtimes?', False, 'timing')
log_info  = PItem('info', 'Log INFO messages?', True, 'log.info')
log_debug = PItem('debug', 'Log Debug messages?', False, 'log.debug')
lp_write  = PItem('lp', 'Write out ILP files?', False, 'measure.lp.write')
profiling = PItem('profiling', 'Profile attribute metrics?', True, 'measure.profiling')
flushed   = PItem('flushed', 'Use strategy "flushed"?', False, 'measure.flush')
noncached = PItem('noncached', 'Use strategy "noncached"?', False, 'measure.non-cached')

items = [timing, log_info, log_debug, lp_write, profiling, flushed, noncached]

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
