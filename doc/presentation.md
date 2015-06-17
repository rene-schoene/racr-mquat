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

- MQuAT concept \cite{gotz2014models}
	- Self-adaptive system optimizing for multiple qualities
	- Component-based design for both hardware and software
	- Quality contracts capturing requirements and guarantees of components
- THEATRE \cite{gotz2010theatre} as a Java-based implementation of MQuAT
	- Knowledge represented with EMF-Models
	- Optimization problem solved by transformation to ILP
	- Designed for distributed operation (see HAECubie) using Master-Slave-Pattern \cite{sahni1996scheduling}

## What was the problem

- ILP-Solution branded "unuseable" for bigger systems
	- current measurements disprove this, see next slides
	- ILP still generated from skretch for each request
	- measurements on Cubieboards still to be done for new approach
- EMF-Model (element)s somewhat ambiguous or superfluous
	- component requirement possible both, on component- and on mode-level
	- structure and variant model contain similar information
	- general approach of structural model not easy to use (especially for ILP-Generation)

\oppcomment{%
- similar information: subcomponents/subresources <br/>%
- general approach not easy to use: assumptions (mostly implicit) made for structural model (e.g. for ILP-Generation: containers are servers with one level of subresources) <br/>%
}

## What was the problem (2)

- Measurement on solving times
	- Context: Java-based System Generator to generate ILP for increasing size of systems, solved by lp\_solve and glpk
	- Format of ILP adopted for glpk via own Python script
- Result
	- lpsolve: 14/23 timed out at 2 minutes, others solved took 0.003 to 80s
	- glpk: all solved within 3 seconds
- Further investigation needed
	- Both solvers do not compute the same solution

## What was the problem (3)

- Even better performance using Gurobi (commercial solver)

\centerline{\includegraphics[width=\linewidth]{../sol.pdf}}

# Ideas and Current State
## What is the idea to solve the problem

- Use RACR \cite{burger2012racr}
	- **R**eference **A**ttribute Grammer **C**ontrolled **R**ewriting
	- specify knowledge as an ASG\footnote{Abstract Syntax Graph, i.e. an Abstract Syntax Tree with references}
	  whose structure is defined by a RAG\footnote{Reference Attribute Grammer}
	- RAG is a combination of structural and variant model, avoiding duplicate information
	- analyses run on ASG now inherently incremental

## How that this improved the existing work

- Hm, not quite there yet:

\centerline{\includegraphics[width=\linewidth]{../gen.pdf}}

## Some Facts

- repository: <https://bitbucket.org/rschoene/racr-mquat>
- IPython Notebook\footnote{http://nbviewer.ipython.org/urls/bitbucket.org/rschoene/racr-mquat/raw/master/ilp-measurement.ipynb} used for measurement plots
- Main language: Scheme
	- implementations used: Racket\footnote{http://racket-lang.org/}, Larceny\footnote{http://www.larcenists.org/}
- Measurement and test scripts: Python

## Code Facts

Using cloc\footnote{http://cloc.sourceforge.net/}

--------------------------------------------------------------------------------
Language                      files          blank        comment           code
-------------------------   -------   ------------   ------------   ------------
Scheme                           13            190            247           2124

Python                            8             77             45            489

Bourne Again Shell                1              0              0              2

SUM:                             22            267            292           2615
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
	- Abstract Interpretation \cite{Cousot1977,Rosendahl1990} to estimate energy consumption \cite{Jayaseelan2006,Rusu2003}
	- Describe decisions \cite{Danylenko2015}
	- Find configurations, which can never be used
	- Unify constraints (in contracts) of modes
- Extend AG
	- Describe multiple systems and their interaction \cite{WSG+2013}
	- Include behaviour model

\oppcomment{%
- vision slides <br/>%
}
