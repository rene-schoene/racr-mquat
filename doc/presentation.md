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

# The past
## Where we started in Phase I

- MQuAT concept \cite{gotz2014models}
	- Self-adaptive system optimizing for multiple qualities
	- Component-based design for both hardware and software
	- Quality contracts capturing requirements and guarantees of components
- THEATRE \cite{gotz2010theatre} as a Java-based implementation of MQuAT
	- Knowledge represented using EMF-(Meta)Models
	- Optimization problem solved by transformation to ILP
	- Designed for distributed operation (see HAECubie) using Master-Slave-Pattern \cite{sahni1996scheduling}

## The optimization problem

Mapping *n* components on *m* resources

\centerline{\includegraphics[width=\linewidth]{opt_problem.pdf}}

## The optimization problem solved

Mapping *n* components on *m* resources

\centerline{\includegraphics[width=\linewidth]{opt_problem_sol.pdf}}

## What was the problem

- Usage of ILP thought *unusable* for bigger systems
	- Current measurements disprove this, see slides 10 and 11
- EMF-Model (element)s somewhat ambiguous or superfluous
	- Component requirement possible on both, component- and mode-level
	- Structure and variant model contain similar information
	- General approach of structural model not easy to use (especially for ILP-Generation)
- Currently, ILP still generated from scratch for each request

\oppcomment{%
- similar information: subcomponents/subresources <br/>%
- general approach not easy to use: assumptions (mostly implicit) made for structural model (e.g. for ILP-Generation: containers are servers with one level of subresources) <br/>%
}

# The present

## How we want to achieve scalability

- Use RACR \cite{burger2012racr}
	- **R**eference **A**ttribute Grammer **C**ontrolled **R**ewriting
	- Specify knowledge as an ASG\footnote{Abstract Syntax Graph, i.e. an Abstract Syntax Tree with references}
	  whose structure is defined by a RAG\footnote{Reference Attribute Grammar}
	- RAG is a combination of structural and variant model, avoiding duplicate information
	- Analyses run on ASG now run inherently **incremental** and are defined **declarative**

## Current state, compared to existing work

- Not quite there yet:

\centerline{\includegraphics[width=\linewidth]{../gen.pdf}}

\oppcomment{%
- Generation with <br/>%
	- Left = Java <br/>%
	- Middle = Racket <br/>%
	- Right = Larceny <br/>%
- 8 seconds maximum <br/>%
- steps during synthetic scenario, increasing amount of changes
}


## Measurement on solving times

- Measurement on solving times
	- Context: System Generator to generate ILP for increasing size of systems
	- 23 different sizes of systems, 40sec timeout
	- Format of ILP adopted for glpk via own Python script
- Result for Java-based solution
	- Timeouts: glpk 10/23, lpsolve 8/23
- Result for RACR-based solution (enhanced ILP)
	- All but one systems solved within 5sec (outlier 12sec)

\oppcomment{%
- Java <br/>%
	- generated format was not accepted by glpk <br/>%
	- syntactical changes only <br/>%
- RACR <br/>%
	- chosen other format for ILP (only binary variables, no floats) <br/>%
	- side result
}

## Measurement on solving times (2)

- Even better performance using Gurobi (commercial solver)

\centerline{\includegraphics[width=\linewidth]{../sol.pdf}}

## Current pitfalls

- Different input formats accepted by lp\_solve and glpk
	- Transformation (mostly syntactical) needed
	- Still, different solution computed (GLPK occasionally ignore binary variables, value e.g. 0.348485)
- Slow running Larceny
	- Unexpected as Larceny compiles to machine code
- Caching not fully exploited
	- Some constraints still unnecessarily recomputed

## General Facts

- <https://bitbucket.org/rschoene/racr-mquat>
- IPython Notebook\footnote{http://nbviewer.ipython.org/urls/bitbucket.org/rschoene/racr-mquat/raw/master/ilp-measurement.ipynb} used for measurement plots
- Main language: Scheme
	- Implementations used: Racket\footnote{http://racket-lang.org/}, Larceny\footnote{http://www.larcenists.org/}
- Measurement and test scripts: Python

## Code Facts

Using cloc\footnote{http://cloc.sourceforge.net/}

--------------------------------------------------------------------------------
Language                      files          blank        comment           code
-------------------------   -------   ------------   ------------   ------------
Scheme                           15            224            316           2287

Python                            8             81             44            509

Bourne Again Shell                1              0              0              2

SUM:                             24            305            360           2798
--------------------------------------------------------------------------------

# The future
## Where we should go next

- Do not transform to ILP
	- Implement an heuristic similar to RACRtune demo\footnote{Shown at HAEC review and OUTPUT'15} of Daniel Langner and Johannes Mey
- Apply static analysis where appropriate, e.g.
	- Abstract Interpretation \cite{Cousot1977,Rosendahl1990} to estimate energy consumption \cite{Jayaseelan2006,Rusu2003}
	- Describe decisions \cite{Danylenko2015}
	- Find configurations, which can never be used
	- Unify constraints (in contracts) of modes
- Extend AG
	- Describe multiple systems and their interaction, e.g. \cite{WSG+2013}
	- Include behavior model for more fine grained description

\oppcomment{%
- vision slides <br/>%
- heuristic different from RACRtune
}

# Presentation TODOs

## todo

arch. picture slide 7
slide 8
	-> 2 folien
	-> overlay glpk(java) + glpk(scheme) + glpk/gurobi)
future -> wcet squeezing
	- slides on website of j.knoop?

## done

where we started
	-> Phase I
different impls slide 5
slide 4 captialize
other wording slide 6
