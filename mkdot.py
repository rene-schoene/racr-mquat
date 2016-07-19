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
