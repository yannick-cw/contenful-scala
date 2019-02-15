package io.yannick_cw.contenful.parser

import java.time.ZonedDateTime

import com.contentful.java.cma.model.{CMALink, CMAType}
import com.google.gson.internal.LinkedTreeMap
import io.yannick_cw.contenful.parser.CmaReader.{
  ParsingError,
  ReadingError,
  Result
}
import io.yannick_cw.contenful.parser.MetaInfo.Coordinates
import shapeless.labelled.{FieldType, field}
import shapeless.{CNil, HList, HNil, LabelledGeneric, Lazy, Witness, :: => #:}

import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.util.Try

package object auto {

  private def baseTypeReader[A](implicit classTag: ClassTag[A]): CmaReader[A] =
    (cursor: CmaCursor) =>
      for {
        focus <- cursor.focus.toRight(
          ReadingError(s"No cursor given to decode ${classTag.toString}")
        )
        anyField <- cursor.scalaFields
          .get(focus)
          .toRight(ReadingError(s"Did not find field $focus"))
        forLocale <- anyField
          .get("de-DE")
          .toRight(ReadingError(s"Did not find $focus for locale de-DE"))
        typedValue <- forLocale match {
          case classTag(a) => Right(a)
          case _ =>
            Left(
              ParsingError(
                s"Could not parse to ${classTag.toString} for $forLocale"
              )
            )
        }
      } yield typedValue

  implicit val boolReader: CmaReader[Boolean]  = baseTypeReader
  implicit val doubleReader: CmaReader[Double] = baseTypeReader
  implicit val intReader: CmaReader[Int]       = doubleReader.map(_.toInt)
  implicit val stringReader: CmaReader[String] = baseTypeReader
  implicit val dateTimeReader: CmaReader[ZonedDateTime] =
    stringReader.emap(
      time =>
        Try(ZonedDateTime.parse(time)).toEither.left.map(
          err => ParsingError(s"Failed parsing time from $time with: $err")
      )
    )

  implicit val coordinatesReader: CmaReader[Coordinates] =
    baseTypeReader[LinkedTreeMap[String, Double]].emap(
      coordsMap =>
        for {
          longitude <- coordsMap.asScala
            .get("lon")
            .toRight(
              ParsingError(
                s"Did not find 'lon' field on Coordinates $coordsMap"
              )
            )
          lattitude <- coordsMap.asScala
            .get("lat")
            .toRight(
              ParsingError(
                s"Did not find 'lat' field on Coordinates $coordsMap"
              )
            )
        } yield
          new Coordinates {
            val lon: Double = longitude
            val lat: Double = lattitude
        }
    )

  private def toLinkType(_type: String): Result[CMAType] = _type match {
    case "Entry" => Right(CMAType.Entry)
    case "Asset" => Right(CMAType.Asset)
    case other   => Left(ParsingError(s"Link type $other is not supported"))
  }
  private def parseCmaLink(
      link: LinkedTreeMap[String, LinkedTreeMap[String, String]]
  ) =
    for {
      sys <- link.asScala
        .get("sys")
        .toRight(ReadingError(s"Could not parese $link to CMALink"))
      scalaMap = sys.asScala
      linkType <- scalaMap
        .get("linkType")
        .toRight(ReadingError(s"Did not find link type on CMALink $link"))
      entry <- toLinkType(linkType)
      id <- scalaMap
        .get("id")
        .toRight(ReadingError(s"Did not find id on CMALink $link"))
    } yield new CMALink(entry).setId(id)

  implicit val cmaLinkReader: CmaReader[CMALink] =
    baseTypeReader[LinkedTreeMap[String, LinkedTreeMap[String, String]]]
      .emap(parseCmaLink)

  implicit def optReader[A](implicit r: CmaReader[A]): CmaReader[Option[A]] =
    (cma: CmaCursor) =>
      r.read(cma)
        .fold({
          case _: ReadingError => Right(None)
          case p: ParsingError => Left(p)
        }, r => Right(Some(r)))

  implicit val javaListReader: CmaReader[java.util.List[AnyRef]] =
    baseTypeReader[java.util.List[AnyRef]]

  private implicit def listReader[A](
      implicit classTag: ClassTag[A]
  ): CmaReader[List[A]] =
    javaListReader.emap(
      jList =>
        jList.asScala.toList
          .map {
            case classTag(value) => Right(value)
            case noMatch =>
              Left(ReadingError(s"Could not parse to $classTag for $noMatch"))
          }
          .foldLeft(Right(List.empty): Result[List[A]])(
            (acc, res) => acc.flatMap(l => res.map(l :+ _))
        )
    )

  implicit val intListReader: CmaReader[List[Int]]       = listReader[Int]
  implicit val stringListReader: CmaReader[List[String]] = listReader[String]
  implicit val boolListReader: CmaReader[List[Boolean]]  = listReader[Boolean]
  implicit val doubleListReader: CmaReader[List[Double]] = listReader[Double]
  implicit val cmaLinkListReader: CmaReader[List[CMALink]] =
    listReader[LinkedTreeMap[String, LinkedTreeMap[String, String]]]
      .emap(
        _.map(parseCmaLink)
          .foldLeft(Right(List.empty): Result[List[CMALink]])(
            (acc, res) => acc.flatMap(l => res.map(_ :: l))
          )
      )

  private def keyTagDecoder[K <: Symbol, V](
      implicit witness: Witness.Aux[K],
      reader: CmaReader[V]
  ): CmaReader[V] =
    CmaReader.instance(
      cursor =>
        reader
          .read(CmaCursor(Some(witness.value.name), cursor.entry))
          .left
          .map(
            err =>
              ReadingError(
                s"Failed for field name ${witness.value.name}: " + err
            )
        )
    )

  implicit val hnilDecoder: CmaReader[HNil] = _ => Right(HNil)

  implicit val cnilDecoder: CmaReader[CNil] = entry =>
    Left(ReadingError(s"Could not parse $entry"))

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
