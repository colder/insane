# INSANE
Interprocedural Shape Analysis Engine for the Scala programming language.  This
project is a plugin for the scala compiler, doing some sort of analysis.

## What does it do?

Insane performs a compositional effect analysis of arbitrary Scala programs and
compute per-method (and per call-context) effect summaries in the form of local
heap graphs.

Insane is a Scala compiler plugin running late in the compiling stage (after
mixins), and is itself composed of multiple sub phases:

 - Contracts Extraction
 - Control Flow Graphs Generation
 - Class Hierarchy Computation
 - Type Analysis
 - Pointer/Effect Analysis


## How to install/use
 - install [sbt](https://github.com/harrah/xsbt/wiki)
 - the first time, use "sbt update" to dowload depencencies
 - run "sbt all"
 - Use "./scalac-insane yourfiles.scala"
