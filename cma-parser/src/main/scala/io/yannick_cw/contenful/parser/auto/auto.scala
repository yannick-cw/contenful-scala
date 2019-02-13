package io.yannick_cw.contenful.parser

import shapeless.labelled.{FieldType, field}
import shapeless.{CNil, HList, HNil, LabelledGeneric, Lazy, Witness, :: => #:}

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

package object auto {

  private def baseTypeParser[A](
      cursor: CmaCursor
  )(implicit classTag: ClassTag[A]): Either[String, A] =
    for {
      focus <- cursor.focus.toRight(
        s"No cursor given to decode ${classTag.toString}"
      )
      anyField <- cursor.scalaFields
        .get(focus)
        .toRight(s"Did not find field $focus")
      forLocale <- anyField
        .get("de-DE")
        .toRight(s"Did not find $focus for locale de-DE")
      typedValue <- forLocale match {
        case classTag(a) => Right(a)
        case _ =>
          Left(s"Could not parse to ${classTag.toString} for $forLocale")
      }
    } yield typedValue

  implicit val intReader: CmaReader[Int] =
    CmaReader.instance(baseTypeParser[Int](_))

  implicit val stringReader: CmaReader[String] =
    CmaReader.instance(baseTypeParser[String](_))

  implicit def optReader[A](implicit r: CmaReader[A]): CmaReader[Option[A]] =
    (cma: CmaCursor) => r.read(cma).fold(_ => Right(None), r => Right(Some(r)))

  implicit val javaListReader: CmaReader[java.util.List[AnyRef]] =
    (cma: CmaCursor) => baseTypeParser[java.util.List[AnyRef]](cma)

  private implicit def listReader[A](
      implicit classTag: ClassTag[A]
  ): CmaReader[List[A]] =
    (cma: CmaCursor) =>
      for {
        jList <- javaListReader.read(cma)
        aList <- jList.asScala.toList
          .map {
            case classTag(value) => Right(value)
            case noMatch         => Left(s"Could not parese to$classTag for $noMatch")
          }
          .foldLeft(Right(List.empty): Either[String, List[A]])(
            (acc, res) => acc.flatMap(l => res.map(_ :: l))
          )
      } yield aList

  implicit val intListReader: CmaReader[List[Int]]       = listReader[Int]
  implicit val stringListReader: CmaReader[List[String]] = listReader[String]

  private def keyTagDecoder[K <: Symbol, V](
      implicit witness: Witness.Aux[K],
      reader: CmaReader[V]
  ): CmaReader[V] =
    CmaReader.instance(
      cursor =>
        reader
          .read(CmaCursor(Some(witness.value.name), cursor.entry))
          .left
          .map(s"Failed for field name ${witness.value.name}: " + _)
    )

  implicit val hnilDecoder: CmaReader[HNil] = _ => Right(HNil)

  implicit val cnilDecoder: CmaReader[CNil] = entry =>
    Left(s"Could not parse $entry")

  implicit def hlistDecoder[K <: Symbol, H, T <: HList](
      implicit
      witness: Witness.Aux[K],
      hReader: Lazy[CmaReader[H]],
      tReader: CmaReader[T]
  ): CmaReader[FieldType[K, H] #: T] =
    CmaReader.instance { entry =>
      for {
        head <- keyTagDecoder[K, H](witness, hReader.value).read(entry)
        tail <- tReader.read(entry)
      } yield #:(field[K](head), tail)
    }

  implicit def genericReader[A, R <: HList](
      implicit G: LabelledGeneric.Aux[A, R],
      R: Lazy[CmaReader[R]]
  ): CmaReader[A] =
    CmaReader.instance(entry => R.value.read(entry).map(G.from))
}
