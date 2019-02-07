import mill._
import mill.scalalib._
import publish._
import ammonite.ops._

import $ivy.`io.github.davidgregory084::mill-tpolecat:0.1.0`
import io.github.davidgregory084.TpolecatModule

object forge extends TpolecatModule {
  def publishVersion = "0.1.0"

  def scalaVersion = "2.12.8"

  override def scalacOptions = T {
    super.scalacOptions() :+ "-Ypartial-unification"
  }

  def scalacPluginIvyDeps = Agg(
    ivy"org.spire-math::kind-projector:0.9.8"
  )

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-mtl-core:0.4.0",
    ivy"io.verizon.quiver::core:7.0.19",
    ivy"io.monix::monix:3.0.0-RC2"
  )

   object test extends Tests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
   }
}