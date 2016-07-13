from utils import local_quiet
try:
	from fabric.api import task
except ImportError:
	from fabric_workaround import task

header = 'digraph G {\nnode [shape = "record"]\n'
footer = '\n}'

@task(default=True)
def convert(f, output=None):
	if output is None:
		output = f+".dot"
	with open(f) as fd:
		lines = fd.readlines()
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
			# define the node of the left hand side
			fd.write('{0} [label = "{{{0} | {1}}}"]\n'.format(lhand, '|'.join(terminals)))
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
