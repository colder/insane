# INSANE
Static program analysis techniques working on object-oriented languages require
precise knowledge of the aliasing relation between variables. This knowledge is
important to, among other things, understand the read and write effects of
method calls on objects.

INSANE is a combination of a pointer analysis with a memory effect analysis for
the Scala programming language. Our analysis is based on abstract
interpretation, it is inter-procedural and flow sensitive. The analysis, aimed
at higher order programs, computes compositional summaries using a very
expressive representation of effects and does not require annotations. This
analysis is implement within a Scala compiler plugin.

## What does it do?

Insane performs a compositional effect analysis of arbitrary Scala programs and
compute per-method (and per call-context) effect summaries in the form of local
heap transformers. There transformers are expressed as graphs.

Insane is a Scala compiler plugin running late in the compiling stage (just
before cleanup), and is itself composed of multiple sub phases:

 - Contracts Extraction
 - Control Flow Graphs Generation
 - Class Hierarchy Computation
 - Type Analysis
 - Pointer/Effect Analysis

## How to install/use
 - Make sure you have [sbt-extras](https://github.com/paulp/sbt-extras) installed
 - Make sure you have a recent Scala build (>=2.10.0-M3) installed at some path P
 - Clone [colder/insane](https://github.com/colder/insane)
 - Update insane's build.sbt to reflect the path to which you have a recent scala, P
 - run "path/to/sbt-extras/sbt -sbt-version 0.12.0-M2 update compile script"
 - Use "./scalac-insane yourfiles.scala"
 - Run "./scalac-insane --help" for usage options:

```
$ ./scalac-insane --help
Usage: scalac-insane <options> <source files>

where standard options include:
 Output Control:
  --ondemand=s1:s2          Only analyze the specified symbols and their dependencies, _ for all
  --dumpcfg=s1:s2           Dumps CFG for the given symbols, _ for all
  --dumppt=s1:s2            Dumps Point-to graphs for the given symbols, _ for all
  --displayta=s1:s2         Displays Type Analysis results for the given symbols, _ for all
  --displaypure=s1:s2       Displays Purity info for the given symbols, _ for all
  --dumphierarchy           Dumps class hierarchy graph
  --dumpcallgraph           Dumps call graph resulting of class analysis
  --dumpcallstats           Dumps stats on call targets refinement
  --verbosity=normal        Sets verbosity (quiet < normal < verbose < debug)
  --verbose                 Sets verbosity to verbose
  --quiet                   Sets verbosity to quiet
  --debug                   Sets verbosity to debug

 Analysis Settings:
  --depthresolution=n       Allocation-site uniqueness depth-resolution, defaults to 1
  --openworld               Do not assume closed world
  --considerpure=s1:s2      Mark certain methods as pure for the analysis
  --considerarbitrary=s1:s2 Flag certain methods as unanalyzable, delaying their analysis
  --inlineStrategy=strat    Use a certain strategy for handling method calls
       Possible Strategies:
         - smart            Delay based on heuristic depending on the precision of a method call
         - inline           Always inline calls => "full" re-usability and efficiency
         - delay            Always delay the analysis =>  "full" precision

 Setting up the environment:
  --config=cfg.xml          Use the provided xml file to configure the access to the database
  --mehrasure               Disable erasure (this will fail rapidly, so beware)

 Miscellaneous:
  --help                 Displays this help

```
