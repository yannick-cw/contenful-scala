import sbt._

object Dependencies {

  val ScalaTestVersion  = "3.0.5"
  val ShapelessVersion  = "2.3.3"
  val ContenfulVersion  = "3.2.4"
  val ScalacheckVersion = "1.14.0"

  lazy val test = Seq(
    "org.scalatest"  %% "scalatest"  % ScalaTestVersion  % Test,
    "org.scalacheck" %% "scalacheck" % ScalacheckVersion % Test
  )

  lazy val shapeless = Seq(
    "com.chuusai" %% "shapeless" % ShapelessVersion
  )

  lazy val contentfulSdk = Seq(
    "com.contentful.java" % "cma-sdk" % ContenfulVersion
  )

  lazy val parserDependencies = test ++ shapeless ++ contentfulSdk
}
