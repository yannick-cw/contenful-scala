package io.yannick_cw.contenful.parser

import com.contentful.java.cma.model.CMAEntry

import scala.collection.JavaConverters._

trait CmaReader[A] {
  def read(cma: CmaCursor): Either[String, A]
  def map[B](f: A => B): CmaReader[B] =
    (cma: CmaCursor) => this.read(cma).map(f)
}

object CmaReader {
  def apply[A](implicit R: CmaReader[A]): CmaReader[A] = R

  def instance[A](func: CmaCursor => Either[String, A]): CmaReader[A] =
    (cma: CmaCursor) => func(cma)
}

abstract class CmaCursor {
  def focus: Option[String]
  def entry: CMAEntry
  def scalaFields: Map[String, Map[String, AnyRef]]
}

object CmaCursor {
  def apply(cFocus: Option[String], cmaEntry: CMAEntry): CmaCursor =
    new CmaCursor {
      val focus: Option[String] = cFocus
      val entry: CMAEntry       = cmaEntry
      val scalaFields: Map[String, Map[String, AnyRef]] =
        cmaEntry.getFields.asScala.toMap.mapValues(_.asScala.toMap)
    }
}
