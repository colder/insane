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
 - Run "./scalac-insane --help" for usage options:

```
$ ./scalac-insane --help
Usage: scalac-insane <options> <source files>

where standard options include:
 Output Control:
  --drawpt=name          Queries the DB and draw corresponding graph
  --dumpcfg=s1:s2        Dumps CFG for the given symbols, _ for all
  --dumppt=s1:s2         Dumps Point-to graphs for the given symbols, _ for all
  --debugfun=s1:s2       Debug given function symbols
  --displayta=s1:s2      Displays Type Analysis results for the given symbols, _ for all
  --displaypure=s1:s2    Displays Purity info for the given symbols, _ for all
  --dumphierarchy        Dumps class hierarchy graph
  --dumpcallgraph        Dumps call graph resulting of class analysis
  --dumpcallstats        Dumps stats on call targets refinement
  --verbosity=normal     Sets verbosity (quiet < normal < verbose < debug)
  --verbose              Sets verbosity to verbose
  --quiet                Sets verbosity to quiet
  --debug                Sets verbosity to debug

 Setting up the environment:
  --config=cfg.xml       Use the provided xml file to configure the access to the database
  --createtables         Initialize the database structure by creating SQL tables
  --fillhierarchy        Fills the database with the class hierarchy computed in this analysis
  --fillgraphs           Fills the database with the graphs computed in this analysis

 Miscellaneous:
  --help                 Displays this help
```
