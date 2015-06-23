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

\centerline{\includegraphics[width=\linewidth]{images/opt_problem.pdf}}

## The optimization problem solved

Mapping *n* components on *m* resources

\centerline{\includegraphics[width=\linewidth]{images/opt_problem_sol.pdf}}

## What was the problem

- Usage of ILP thought *unusable* for bigger systems
	- Current measurements disprove this, see slides 11 – 13
- EMF-Models and some of their elements ambiguous or redudant
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

## Test setup

\centerline{\includegraphics[width=\linewidth]{images/gen_sol_arch.pdf}}

\(a) Initial creation, (b) HW changes

## Measurements

- Setup
	- System Generator to generate ILP for increasing size of systems
	- 23 different sizes of systems
	- ILP-Solving with 40sec timeout
	- Two new aspects
		- technology used: Java vs. RACR
		- ILP format: old vs. enhanced
- ILP-Solving using existing Java-based ILP format
	- Timeouts: glpk 10/23, lpsolve 8/23
- ILP-Solving using RACR-based enhanced ILP format
	- All but one systems solved within 5sec (outlier 12sec)

\oppcomment{%
- Java <br/>%
	- generated format was not accepted by glpk <br/>%
	- syntactical changes only <br/>%
- RACR <br/>%
	- chosen other format for ILP (only binary variables, no floats) <br/>%
	- side result
}

## Measurement on generation times

- Not quite there yet:

\centerline{\includegraphics[height=0.6\textheight]{images/gen.pdf}}

\oppcomment{%
- Generation with <br/>%
	- Left = Java <br/>%
	- Middle = Racket <br/>%
	- Right = Larceny <br/>%
- 8 seconds maximum <br/>%
- steps during synthetic scenario, increasing amount of changes
}

## Measurement on solving times (2)

- Promising results

<!--\centerline{\includegraphics[width=\linewidth]{images/sol.pdf}}-->
\centerline{\includegraphics[height=0.6\textheight]{images/sol.pdf}}


## Measurement on solving times (3)

\centerline{\includegraphics[height=0.5\textheight]{images/comp_sol.pdf}}

- solid = GLPK, old format, dashed = GLPK, enhanced format
- dotted = Gurobi, enhanced format ($\leq$ 1sec)

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
- Main language: Scheme
	- Implementations used: Racket\footnote{http://racket-lang.org/}, Larceny\footnote{http://www.larcenists.org/}

--------------------------------------------------------------------------------
Language                      files          blank        comment           code
-------------------------   -------   ------------   ------------   ------------
Scheme                          14            203            300           2207

Python                           7             75             43            477

SUM:                            21            278            343           2684
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

## An example application of static analysis

WCET squeezing \cite{knoop2013wcet}

- Combines ILP solving with symbolic execution \cite{King1976} (SE)
- Iterative, alternating, automatic approach
- SE either tighten found bound	– or proves it precise

Application to HAEC use case

- Do WCEC squeezing
	- worst case energy consumption
	- based on energy contracts
