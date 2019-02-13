// Your profile name of the sonatype account. The default is the same with the organization value
sonatypeProfileName in ThisBuild := "io.github.yannick-cw"

// To sync with Maven central, you need to supply the following information:
publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

// License of your choice
licenses in ThisBuild := Seq(
  "MIT" -> url("http://www.opensource.org/licenses/mit-license.php")
)
homepage in ThisBuild := Some(
  url("https://github.com/yannick-cw/contentful-scala/")
)
scmInfo in ThisBuild := Some(
  ScmInfo(
    url("https://github.com/yannick-cw/contenful-scala/"),
    "scm:git@github.com:yannick-cw/contenful-scala.git"
  )
)
developers in ThisBuild := List(
  Developer(
    id = "yannick-cw",
    name = "Yannick Gladow",
    email = "yannick.gladow@gmail.com",
    url = url("https://github.com/yannick-cw")
  )
)
