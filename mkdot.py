from utils import local_quiet
try:
	from fabric.api import task
except ImportError:
	from fabric_workaround import task

header = 'digraph G {\nnode [shape = "record"]\n'
footer = '\n}'

def ata(d, nonterminal, attribute):
	d.setdefault(nonterminal, []).append(attribute)

@task
def test_pa(*attribute_files):
	"""
	Test parse_atts
	"""
	print parse_atts(attribute_files)

def parse_atts(attribute_files):
	# stages
	agsearch, rulenamesearch, nonterminalsearch = 1,2,3
	stage = agsearch
	d = {}
	for f in attribute_files:
		with open(f) as fd:
			lines = fd.readlines()
		for line in lines:
			line = line.partition(';')[0].strip()
			if line.startswith('(ag-rule'): ## stage == agsearch and
				rule_index = line.index('ag-rule')
				if line[rule_index+8:]:
					# there is more on this line
					bracket_index = line.find('(', rule_index+8)
					if bracket_index == -1:
						# only the name of the attribute
						rulename = line[rule_index:bracket_index].strip()
					else:
						rulename, nonterminal = [x.strip() for x in line[rule_index+8:].split('(')[0:2]]
						ata(d,nonterminal,rulename)
					stage = nonterminalsearch
				else:
					stage = rulenamesearch
			elif stage == rulenamesearch:
				if '(' in line:
					# there is something more on the line
					rulename, _, rest = line.partition('(')
					nonterminal = rest.strip().split()[0]
					ata(d,nonterminal,rulename)
				else:
					# name is on one line
					rulename = line.partition(';')[0].strip()
				stage = nonterminalsearch
			elif stage == nonterminalsearch:
				bracket_index = line.find('(')
				if bracket_index > -1 and line[bracket_index+1].isupper():
					nonterminal = line[bracket_index+1:].strip().split()[0]
					ata(d,nonterminal,rulename)
	return d

def mk_balanced_atts(attributes, max_length = 25):
	result = [] # empty list of lists
	sums   = [] # empty list of sums
	attributes.sort(key=len, reverse=True)
	for a in attributes:
		# search through result lists
		# if one list has enough space (i.e. after insertion, the sum of lengths is <= than max_length)
		# if not, create a new chunk
		# in each case, update the sums
		l = len(a)
		for i, chunk in enumerate(result):
			if sums[i] + l <= max_length:
				# add to this chunk
				chunk.append(a)
				sums[i] = sums[i] + l
				break
		else:
			# if no chunk fits, create a new one and update sums
			result.append([a])
			sums.append(l)
	return result

@task
def test_ba():
	"""
	Test mk_balanced_atts
	"""
	print mk_balanced_atts([str(i) for i in xrange(12)])
	print mk_balanced_atts([str(i) for i in xrange(34)])

	print mk_balanced_atts(['check-model','to-ilp','ilp-objective','ilp-nego','ilp-binary-vars','clauses-met?','every-container','every-pe','every-res-type','every-comp','every-impl','every-mode','every-sw-clause','every-hw-clause','lookup-property','objective-name','objective-value','get-request','get-root','search-comp','search-pe','target','type'])
	print mk_balanced_atts(dict2list({'comp': 'eq', 'value': '#<procedure:...st-generation.ss:28:39>'}))

@task(default=True)
def run():
	"""
	Create default grammar dot file
	"""
	convert('ast.scm', True, 'ilp.scm', 'basic-ag.scm')

def convert(f, balance_atts, *atts):
	"""
	Converts the given RAG specification into a dot file
	@f: File containing AST rules
	@balance_atts: True to break attributes into chunks with similar length
	@atts: Files containing AG rules
	"""
	output = f+".dot"
	with open(f) as fd:
		lines = fd.readlines()
	d_attributes = parse_atts(atts)
	with open(output, 'w') as fd:
		fd.write(header)
		for line in [l.strip() for l in lines if l.strip().startswith('(ast-rule')]:
			rule = line[line.index('(ast-rule \'')+11:line.rindex(')')]
			# inheritance-arrows are "arrowhead = empty"
			print rule
			lhand, rhand = rule.split('->')
			superclass   = lhand[lhand.index(':')+1:] if ':' in lhand else None
			if superclass:
				lhand = lhand[:lhand.index(':')]
			children     = rhand.split('-') if rhand.strip() else []
			terminals    = filter(lambda x: x.islower(), children)
			nonterminals = filter(lambda x: not x.islower(), children)
			attributes   = d_attributes.get(lhand, [])
			attribute_string = '{ ' + ' } | { '.join([' | '.join(l) for l in mk_balanced_atts(attributes)]) + ' }' if balance_atts else ' | '.join(attributes)
			# define the node of the left hand side
			fd.write('{0} [label = "{{{0} | {{ t | {1} }} | {{ a | {2} }} }}"]\n'.format(lhand, ' | '.join(terminals), attribute_string))
			# add inheritance
			if superclass:
				fd.write('{0} -> {1} [arrowhead = "empty"]\n'.format(lhand, superclass))
			# add nonterminal children
			for child in nonterminals:
				print ' child:', child
				options = ''
				# handle contexts
				if '<' in child:
					child, context = child.split('<')
					options = 'label = "{0}"'.format(context)
				# handle lists
				if child.endswith('*'):
					child = child[:-1]
					if options:
						# it had a context
						options = 'headlabel = "{0}", label = "{1}"'.format('*', context)
					else:
						options = 'headlabel = "*"'
				fd.write('{0} -> {1}[arrowtail = diamond, dir = back, {2}]\n'.format(lhand, child, options))
		fd.write(footer)
	local_quiet('dot {0} -Tpdf > {0}.pdf'.format(output))

t_new_nt   = '-\\'
t_new_list = '-*'
t_level    = ' |'
t_terminal = '|- '
t_attr     = '| <'
s_list     = '>*'
debugging  = False

def dict2list(d, sep=':'):
	return ['{0}{1}{2}'.format(key,sep,value) for key,value in d.iteritems()]

def clean(value):
	return value.strip().replace('{', '(').replace('}', ')')

@task
def mkast(f):
	output = f + '.dot'
	d = {} # node-name -> [(terminal-value-list, attribut-value-list)]
	with open(f) as fd:
		lines = fd.readlines()
	level = 0  # current, expected indendation level
	nid   = 0  # node id
	stack = [] # stack of node-names
	current_node = ''
	for i, line in enumerate(lines):
		try:
			read_level = line.count(t_level)
			if read_level < level:
				# some node was finished, do something
				if debugging:
					print '> Level adaptation at line', i+1
					print line.rstrip()
					print stack
					print "read_level:", read_level, "level:", level, "current:", current_node
				for _ in xrange(level - read_level): ## FIXME: maybe add 1 to this
					current_node = stack.pop()
				level = read_level
#				if current_node.startswith(s_list):
#					print '>> List-Hack for', current_node
#					level -= 1
#					current_node = stack.pop()
			if t_new_nt in line:
				# TODO: also make a link to previous node
				stack.append(current_node)
				current_node = '{0}-{1}'.format(line[line.find(t_new_nt)+2:].strip(),nid)
				nid   += 1
				level += 1
				d.setdefault(current_node, [{},{},stack[-1]])
				if debugging:
					print '> new node:', current_node, 'stack:', stack
			elif t_new_list in line:
				# TODO: somehow remember current name of list nonterminal
				stack.append(current_node)
				level += 1
				name = line[line.find(t_new_list)+3:].strip()
#				stack.append(s_list + name)
				if debugging:
					print '> found list in', current_node, ':', name, 'stack:', stack
				pass
			elif t_terminal in line:
				key, _, value = line[line.find(t_terminal)+3:].partition(':')
				if debugging:
					print '> found terminal in', current_node, ':', key.strip(), '==', value.strip()
				d[current_node][0][key.strip()] = clean(value)
			elif t_attr in line:
				key, _, value = line[line.find(t_attr)+3:].partition('>')
				if debugging:
					print '> found attribute in', current_node, ':', key, '=>', value.strip()
				d[current_node][1][key] = clean(value)
		except:
			if debugging:
				print ">> Error in line", i+1
				print stack
			raise

	if debugging:
		print(d)
	with open(output, 'w') as fd:
		fd.write(header)
		for nodename,values in d.iteritems():
			# define the hierachy
			nodename = nodename.replace('-', '_')
			if values[2]:
				parent = values[2].replace('-', '_')
				fd.write('{0} -> {1}\n'.format(parent, nodename))
			terminal_string  = '{ ' + ' } | { '.join([' | '.join(l) for l in mk_balanced_atts(dict2list(values[0]))]) + ' }'
			attribute_string = '{ ' + ' } | { '.join([' | '.join(l) for l in mk_balanced_atts(dict2list(values[1]))]) + ' }'
			# define the node
			fd.write('{0} [label = "{{{0} | {{ t | {1} }} | {{ a | {2} }} }}"]\n'.format(nodename, terminal_string, attribute_string))
		fd.write(footer)
	local_quiet('dot {0} -Tpdf > {0}.pdf'.format(output))
