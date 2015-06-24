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

\oppcomment{%
- MQuAT: approach to build self-adaptive systems <br/>%
- model-driven, component-based, use quality contracts <br/>%
- THEATRE: reference implementation <br/>%
- use EMF-models, optimization problem (shown later) solved with ILP
}

## Structure and variant model

\centerline{\includegraphics[height=0.7\textheight]{images/models.png}}

\oppcomment{%
- structure: defines types of possible components <br/>%
- variant: runtime-model (only shown for hardware here) <br/>%
}

## The optimization problem

Mapping *n* components on *m* resources

\centerline{\includegraphics[width=\linewidth]{images/opt_problem.pdf}}

\oppcomment{%
- simple architecture shown here <br/>%
- n software component, aligned in pipe-and-filter, each 2 impls, 2 modes <br/>%
- modes have contracts <br/>%
- m hardware components (workers) <br/>%
- request triggers optimization
}

## The optimization problem solved

Mapping *n* components on *m* resources

\centerline{\includegraphics[width=\linewidth]{images/opt_problem_sol.pdf}}

\oppcomment{%
- choose mapping of software components onto hardware components <br/>%
- such that requirements are fulfilled and optimal with regard to objective function
}

## What was the problem

- Usage of ILP thought *unusable* for bigger systems
	- Current measurements disprove this, see slides 11 – 13
- EMF-Models and some of their elements ambiguous or redudant
	- Component requirement possible on both, component- and mode-level
	- Structure and variant model contain similar information
	- Approach of structural model not easy to use (especially for ILP-Generation)
- Currently, ILP generated from scratch for each request

\oppcomment{%
- ILP believed to be unsusable, see slides 11-13 <br/>%
- similar information: subcomponents/subresources <br/>%
- general approach not easy to use: assumptions (mostly implicit) made for structural model (e.g. for ILP-Generation: containers are servers with one level of subresources) <br/>%
- generation of ILP always from scratch
}

# The present

## How we want to achieve scalability

- Use RACR \cite{burger2012racr}
	- **R**eference **A**ttribute Grammer **C**ontrolled **R**ewriting
	- Specify knowledge as an ASG\footnote{Abstract Syntax Graph, i.e. an Abstract Syntax Tree with references}
	  whose structure is defined by a RAG\footnote{Reference Attribute Grammar}
	- RAG is a combination of structural and variant model, avoiding duplicate information
	- Analyses run on ASG now run inherently **incremental** and are defined **declaratively**

\oppcomment{%
- use compiler techniques: RAGs and graph rewriting <br/>%
- tool = RACR, allows to specify AST and attributes, enables incremental analysis <br/>%
- here: structure + variant model combined to single ASG <br/>%
- attributes to compute ILP
}

## Test setup

\centerline{\includegraphics[width=\linewidth]{images/gen_sol_arch.pdf}}

\(a) Initial creation, (b) HW changes

\oppcomment{%
- want to measure time to generate and to solve ILP <br/>%
- (a) create a synthetic model, generate ILP, solve it <br/>%
- (b) simulate hardware changes, generate and solve again <br/>%
- assumption: software does not change (that frequently)
}

## Measurements

- Setup
	- System Generator to generate ILP for increasing size of systems
	- 23 different sizes of systems
	- ILP-Solving with 40sec timeout
\vfill
- ILP-Solving using existing Java/EMF-based, old ILP format
	- Timeouts: glpk 10/23, lpsolve 8/23
\vfill
- ILP-Solving using Scheme/RACR-based, enhanced ILP format
	- All but one systems solved within 5sec (outlier 12sec)

\oppcomment{%
- Java <br/>%
	- same problem solved, but 40% timed out
- RACR <br/>%
	- chosen other format for ILP (only binary variables, no floats) <br/>%
	- side result next slides
}

## Measuring generation times

- Not quite there yet:

\centerline{\includegraphics[height=0.6\textheight]{images/gen.pdf}}

\oppcomment{%
- Generation with <br/>%
	- Left = Java <br/>%
	- Middle = Racket <br/>%
	- Right = Larceny <br/>%
- 8 seconds maximum on y axis <br/>%
- steps during synthetic scenario, increasing amount of changes
}

## Measuring solving times

- Promising results

\centerline{\includegraphics[height=0.6\textheight]{images/sol.pdf}}

\oppcomment{%
- side result from new format of ILP encoding the problem <br/>%
}

## Measuring solving times (Detailed)

\centerline{\includegraphics[height=0.65\textheight]{images/comp_sol.pdf}}

- solid = GLPK, old format, dashed = GLPK, enhanced format
- dotted = Gurobi, enhanced format ($\leq$ 1sec)

\oppcomment{%
- already big change from old to new format <br/>%
- even more using commercial solver Gurobi
}

## Current pitfalls

- Different input formats accepted by lp\_solve and glpk
	- Transformation (mostly syntactical) needed
- Slow running Larceny
	- Unexpected as Larceny compiles to machine code
- Caching not fully exploited
	- Some constraints still unnecessarily recomputed

\oppcomment{%
- Difficult to compare solvers due to different accepted Syntax <br/>%
- Larceny expected to run faster than Racket, but does not <br/>%
- Only first implementation, improvements possible and likely
}

## General Facts

- <https://bitbucket.org/rschoene/racr-mquat>
- Main language: Scheme

--------------------------------------------------------------------------------
Language                      files          blank        comment           code
-------------------------   -------   ------------   ------------   ------------
Scheme                          12            159            168           1222

Python                           6             49             16            284

SUM:                            19            212            185           1546
--------------------------------------------------------------------------------

\oppcomment{%
- java ca. 224kloc, just ILP-generator around same size as complete scheme <br/>%
- reasonable big test suite of ILP-generator testing for correctness
}

# The future
## Where we should go next

- Do not transform to ILP
	- Implement a heuristic similar to RACRtune demo\footnote{Shown at HAEC review and OUTPUT'15, paper in progress} of Daniel Langner and Johannes Mey
- Apply static analysis where appropriate, e.g.
	- Abstract Interpretation \cite{Cousot1977,Rosendahl1990} to estimate energy consumption \cite{Jayaseelan2006,Rusu2003}
	- Describe decisions \cite{Danylenko2015}
	- Eliminate unreachable configurations
	- Unify constraints (in contracts) of modes
- Extend AG
	- Describe multiple systems and their interaction, e.g. \cite{WSG+2013}
	- Include behavior model for more fine grained description

\oppcomment{%
- vision slides <br/>%
- heuristic different from RACRtune <br/>%
- but expected to be faster than ILP, directly using features of RACR <br/>%
- static analysis (Phase II), abstract interpretation of program <br/>%
- formally describe decisions, eliminate unreachable configurations, unify constraints <br/>%
- could also go in direction of multiple systems
}

## An example application of static analysis

Worst Case Execution Time (WCET) squeezing \cite{knoop2013wcet}

- Combines ILP solving with Symbolic Execution (SE) \cite{King1976}
- Iterative, alternating, automatic approach
- SE either tightens found bound – or proves it precise

\vfill
Application to HAEC use case

- Do Worst Case Energy Consumption (WCEC) squeezing
	- based on energy contracts

\oppcomment{%
- approach of Jens Knoop, estimates WCET using ILP <br/>%
}
