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

  def ivyDeps = Agg(
    ivy"io.monix::monix:3.0.0-RC2"
  )

   object test extends Tests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
   }
}