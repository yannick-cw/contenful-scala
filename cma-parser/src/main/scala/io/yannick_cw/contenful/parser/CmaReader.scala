package io.yannick_cw.contenful.parser

import java.util

import com.contentful.java.cma.model.CMAEntry
import io.yannick_cw.contenful.parser.CmaReader.Result

import scala.collection.JavaConverters._

trait CmaReader[A] {
  def read(cma: CmaCursor): Result[A]
  def map[B](f: A => B): CmaReader[B] =
    (cma: CmaCursor) => this.read(cma).map(f)
  def emap[B](f: A => Result[B]): CmaReader[B] =
    (cma: CmaCursor) => this.read(cma).flatMap(f)
}

object CmaReader {
  sealed abstract class Error extends Exception {
    final override def fillInStackTrace(): Throwable = this
  }

  final case class ReadingError(message: String) extends Error {
    override def toString: String   = s"ReadingFailure: $message"
    override def getMessage: String = message
  }
  final case class ParsingError(message: String) extends Error {
    override def toString: String   = s"ParsingError: $message"
    override def getMessage: String = message
  }

  type Result[A] = Either[Error, A]
  def apply[A](implicit R: CmaReader[A]): CmaReader[A] = R

  def instance[A](func: CmaCursor => Result[A]): CmaReader[A] =
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
        Option(cmaEntry.getFields)
          .getOrElse(new util.LinkedHashMap())
          .asScala
          .toMap
          .mapValues(_.asScala.toMap)
    }
}
