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
output:
  beamer_presentation:
    theme: "AnnArbor"
---


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


## What was the idea to solve the problem

- Use RACR
	- specify knowledge as an ASG\footnote{Abstract Syntax Graph, i.e. an Abstract Syntax Tree with references}
	  whose structure is defined by an RAG\footnote{Reference Attribute Grammer}
	- RAG is a combination of structural and variant model, avoiding duplicate information
	- analyses run on ASG now inherently incremental

## How that this improved the existing work

- Hm, not quite there yet:

\centerline{\includegraphics[width=1.2\linewidth]{../gen.png}}

## Where we should go next

- Do not transform to ILP
	- Implement an heuristic similar to HAEC demo of Daniel and Johannes
- Apply static analysis where appropriate, e.g.
	- Find configurations, which can never be used
	- Unify constraints (in contracts) of modes

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
