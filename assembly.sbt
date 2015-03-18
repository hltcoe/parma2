
import AssemblyKeys._

assemblySettings

jarName in assembly := "parma-via-assembly.jar"

mergeStrategy in assembly <<= (mergeStrategy in assembly) { old => {
  case PathList("/home/hltcoe/twolfe/fall2013/data") | PathList("/home/hltcoe/twolfe/fall2013/diagnostics") =>
    MergeStrategy.discard
  case "LICENSE" | "LICENSE.txt" | "NOTICE" | "license" | "pom.xml" =>
  	MergeStrategy.discard
  case "JWNLResource_en.properties" => MergeStrategy.first
  case PathList(ps @ _*) if !ps.forall(!_.contains("snowball")) => MergeStrategy.first
  case PathList(ps @ _*) if !ps.forall(!_.contains("jdom")) => MergeStrategy.first
  case PathList(ps @ _*) if !ps.forall(!_.contains("jwnl")) => MergeStrategy.first
  case PathList(ps @ _*) if !ps.forall(!_.contains("stanford")) => MergeStrategy.first
  case PathList(ps @ _*) if !ps.forall(!_.contains("google")) => MergeStrategy.first
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.discard
  case PathList("org", "apache", "commons", xs @ _*) => MergeStrategy.last
  case PathList("org", "w3c", "dom", xs @ _*) => MergeStrategy.last
  case x => old(x)
  }
}

// skip tests
test in assembly := {}

jarName in assembly := "parma-via-assembly.jar"

mainClass in assembly := Some("edu.jhu.hlt.parma.CLI")

