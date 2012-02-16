resolvers += "spray repo" at "http://repo.spray.cc"

libraryDependencies ++= Seq(
	"net.databinder" %% "dispatch-http" % "0.8.5",
	"net.liftweb" %% "lift-json" % liftVersion,
	"net.liftweb" %% "lift-util" % liftVersion,
	"org.bouncycastle" % "bcprov-jdk16" % "1.46",
	"org.bouncycastle" % "bcpg-jdk16" % "1.46",
	"com.decodified" %% "scala-ssh" % "0.5.0",
    "org.slf4j" % "slf4j-api" % "1.6.4",
	"org.scalatest" %% "scalatest" % "1.6.1" % "test"
)
