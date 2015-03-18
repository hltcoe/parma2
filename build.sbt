import BuildProject._

import AssemblyKeys._

//net.virtualvoid.sbt.graph.Plugin.graphSettings

resolvers ++= Seq(
  "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
  "HLTCOE Maven Repository libs snapshots repo" at "http://localhost:8082/artifactory/repo",
  "HLTCOE Maven Repository libs release" at "http://localhost:8082/artifactory/libs-release-local",
  "HLTCOE Maven Repository libs snapshots" at "http://localhost:8082/artifactory/libs-snapshot-local"
)

name := "parma"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "edu.jhu.hlt.concrete" % "concrete-protobufs" % "1.2.0",
  "edu.jhu.hlt.concrete" % "concrete-java" % "1.2.0",
  "edu.jhu.hlt.phylo" % "transduce" % "1.0.3",
  "edu.jhu.jerboa" % "jerboa" % "1.0.1",
  "edu.jhu.coe.cale" % "cale" % "1.0",
  "com.googlecode.json-simple" % "json-simple" % "1.1.1",
  "com.google.protobuf" % "protobuf-java" % "2.5.0",
  "redis.clients" % "jedis" % "2.0.0",
  "cc.mallet" % "mallet" % "2.0.7-RC2",
  "edu.mit" % "jwi" % "2.2.3",
  "colt" % "colt" % "1.2.0",
  "no.priv.garshol.duke" % "duke" % "1.0")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList(ps @ _*) if ps.last endsWith "Lambda.class" => MergeStrategy.first
	case PathList(ps @ _*) if ps.map(_.toString).contains("trove") => MergeStrategy.first
	case PathList(ps @ _*) if ps.map(_.toString).contains("prim") => MergeStrategy.first
	case PathList(ps @ _*) if ps.map(_.toString).contains("cern") => MergeStrategy.first
    case x => old(x)
  }
}


fork in run := true

javaOptions += "-Djava.library.path=/export/common/SCALE13/Text/parma-data/cplex/bin/x86-64_sles10_4.1/"

//javaOptions += "-Djava.library.path=/home/travis/code/parma-data/cplex/bin/x86-64_sles10_4.1/"

javaOptions += "-Xmx9G"

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-language:postfixOps"

scalacOptions += "-language:implicitConversions"

scalacOptions in (Compile, console) += "-Yrepl-sync"

mainClass in assembly := Some("edu.jhu.hlt.parma.CLI")

mainClass in (Compile, packageBin) := Some("edu.jhu.parma.CLI")

// I want to be able to move a source file from src/main/ into the
// root of the project and have sbt not compile it.
// This means that sbt will only compile files in the src/ directory.
sources in (Compile, compile) ~= (_ filter (_.getPath contains "src/"))

// don't compile maxmargin package because it depends on cplex
//sources in (Compile, compile) ~= (_ filter (f => !f.getPath.contains("src/main/scala/edu/jhu/hlt/parma/inference/maxmargin")))

// in console it is nice to not have to import stuff over-and-over again
initialCommands := """
  import edu.jhu.hlt.parma.types._
  import edu.jhu.hlt.parma.util._
  import edu.jhu.hlt.parma.util.Reservoir._
  import edu.jhu.hlt.parma.util.MiscImplicits._
  import edu.jhu.hlt.parma.math._
  import edu.jhu.hlt.parma.input._
  import edu.jhu.hlt.parma.inference._
  import edu.jhu.hlt.parma.inference.maxmargin._
  import edu.jhu.hlt.parma.experiments._
  import edu.jhu.hlt.parma.features._
  import edu.jhu.hlt.parma.evaluation._
  import edu.jhu.hlt.parma.feature_interfaces._
  import edu.jhu.hlt.parma.diagnostics._
  import edu.jhu.hlt.parma.annotation._
  import java.io.File
  import collection.mutable.ArrayBuffer
  ParmaConfig.load("parma.config")
"""


