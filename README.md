parma2
======

Parma is a Predicate ARguMent Alignment tool, described in the following
publications:

Predicate Argument Alignment using a Global Coherence Model
Travis Wolfe, Mark Dredze, Benjamin Van Durme. NAACL 2015

[PARMA: A Predicate Argument Aligner](http://www.aclweb.org/anthology/P13-2012)
Travis Wolfe, Benjamin Van Durme, Mark Dredze, Nicholas Andrews, Charley Beller,
Chris Callison-Burch, Jay DeYoung, Justin Snyder, Jonathan Weese, Tan Xu, and Xuchen Yao. ACL 2013.

Dependencies
------------

Parma2 requires [CPLEX](http://www-01.ibm.com/software/commerce/optimization/cplex-optimizer/)
in order to perform joint inference (described in the NAACL paper). The local model can be
run wihtout CPLEX. Commercial licences for CPLEX are expensive, but researchers can sign
up for a free academic license.

Parma2 also requires [CAEVO](https://github.com/nchambers/caevo) for the temporal event ordering
feature described in the NAACL paper.

After you have those jars (and put them in a lib/ directory),
the other dependencies are handled by sbt.

The data for running everything can be found [here](http://www.cs.jhu.edu/~travis/data/parma-data.tgz)
and should be put in (or symlinked to) a directory in the project folder called data.

