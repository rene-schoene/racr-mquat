---
title: RACR-MQuAT – The journey has just begun
author:
- René Schöne
header-includes:
	- \usepackage{lscape}
	- \usepackage{lipsum}
	- \newcommand{\blandscape}{\begin{landscape}}
	- \newcommand{\elandscape}{\end{landscape}}
comment:
	- \setupexternalfigures[factor=fit]
---

# Start and Problems
## Where we started

- THEATRE as a Java-based implementation of MQuAT
	- Knowledge represented with EMF-Models
	- Optimization problem solved by transformation to ILP
	- capable of distributed operation (see HAECubie) using Master-Slave-Pattern [ref?]

## What was the problem

- ILP-Solution branded "unuseable" for bigger systems
	- current measurements disprove this, see next slide
	- generation however still started from skretch for each request
		- measurements on Cubieboards still todo for new approach
- EMF-Model (element)s somewhat ambiguous or superfluous
	- component requirement possible both, on component- and on mode-level
	- structure and variant model contain similar information
	- general approach of structural model not easy to use (especially for ILP-Generation)

## What was the problem (2)

- Quick measurement
	- context: Java-based System Generator to generate ILP, solved by lp\_solve and glpk
	- format of ILP needed to be adopted for glpk ← Python script
- Result
	- bad solver (lp\_solve): 14/23 timed out at 2 minutes, others solved took 0.003–80s
	- good solver (glpk): all solved within 3 seconds
- Further investigation needed
	- both solvers did not compute the same solution

## What was the problem (3)

- Even better performance using Gurobi (commercial solver)

\centerline{\includegraphics[width=1.2\linewidth]{../sol.pdf}}

# Ideas and Current State
## What was the idea to solve the problem

- Use RACR
	- specify knowledge as an ASG\footnote{Abstract Syntax Graph, i.e. an Abstract Syntax Tree with references}
	  whose structure is defined by an RAG\footnote{Reference Attribute Grammer}
	- RAG is a combination of structural and variant model, avoiding duplicate information
	- analyses run on ASG now inherently incremental

## How that this improved the existing work

- Hm, not quite there yet:

\centerline{\includegraphics[width=1.2\linewidth]{../gen.pdf}}

## Some Facts

- repository: <https://bitbucket.org/rschoene/racr-mquat>
- IPython [Notebook](http://nbviewer.ipython.org/urls/bitbucket.org/rschoene/racr-mquat/raw/master/ilp-measurement.ipynb) used for measurement plots
- Main language: Scheme
	- implementations used: [Racket](http://racket-lang.org/), [Larceny](http://www.larcenists.org/)
- Measurement and test scripts: Python

## Code Facts

--------------------------------------------------------------------------------
Language                      files          blank        comment           code
-------------------------   -------   ------------   ------------   ------------
Scheme                           16            210            248           2203

Python                            8             77             45            489

Bourne Again Shell                1              0              0              2

SUM:                             25            287            293           2694
--------------------------------------------------------------------------------

## Current pitfalls

- different input formats accepted by lp\_solve and glpk
	- transformation (mostly syntactical) needed
	- still, different solution computed (GLPK occasionally let binary variable's value e.g. 0.348485)
- slow running Larceny
	- quite unexpected as Larceny compiles to machine code


# The future
## Where we should go next

- Do not transform to ILP
	- Implement an heuristic similar to HAEC demo of Daniel and Johannes
- Apply static analysis where appropriate, e.g.
	- Find configurations, which can never be used
	- Unify constraints (in contracts) of modes
- Extend AG
	- Describe multiple systems and their interaction
	- Include behaviour model

---

# Notes

## Where we started

## What was the problem

- similar information: subcomponents/subresources
- general approach not easy to use: assumptions (mostly implicit) made for structural model (e.g. for ILP-Generation: containers are servers with one level of subresources)

## What was the idea to solve the problem

## How that this improved the existing work

## Where we should go next

- vision slides
