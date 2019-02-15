package io.yannick_cw.contenful.parser

import java.time.ZonedDateTime

import com.contentful.java.cma.model.CMAEntry
import io.yannick_cw.contenful.parser.CmaReader.{ParsingError, ReadingError}

import scala.util.Try

object MetaInfo {
  sealed trait Status
  case object Draft     extends Status
  case object Archived  extends Status
  case object Published extends Status
  case object Updated   extends Status

  object Status {
    // The act of publishing an entity increases its version by 1, so any entry which has
    // 2 versions higher or more than the publishedVersion has unpublished changes.
    def isUpdated(entry: CMAEntry): Boolean =
      Option(entry.getSystem.getPublishedVersion).exists(
        publishedVersion => entry.getSystem.getVersion > publishedVersion + 1
      )

    implicit val entryReader: CmaReader[Status] = (cma: CmaCursor) =>
      if (cma.entry.isArchived == true) Right(Archived)
      else if (cma.entry.isPublished) Right(Published)
      else if (isUpdated(cma.entry)) Right(Updated)
      else Right(Draft)
  }

  trait Coordinates {
    override def toString: String = s"Coordinates { lon: $lon, lat: $lat }"
    val lon: Double
    val lat: Double
  }

  private def parseTime[A](
      time: Option[String],
      name: String,
      createA: ZonedDateTime => A
  ) =
    time
      .toRight(ReadingError(s"Did not find a time for $name"): CmaReader.Error)
      .flatMap(
        stringTime =>
          Try(ZonedDateTime.parse(stringTime)).toEither
            .map(createA)
            .left
            .map(
              err =>
                ParsingError(
                  s"Could not parse time for $name: $stringTime with $err"
                )
            )
      )

  trait CreatedAt {
    val date: ZonedDateTime
    override def toString: String = date.toString
  }
  object CreatedAt {
    implicit val createdReader: CmaReader[CreatedAt] = (cma: CmaCursor) =>
      parseTime(
        Option(cma.entry.getSystem.getCreatedAt),
        "CreatedAt",
        time =>
          new CreatedAt {
            val date: ZonedDateTime = time
          }
      )
  }

  trait UpdatedAt {
    val date: ZonedDateTime
    override def toString: String = date.toString
  }
  object UpdatedAt {
    implicit val reader: CmaReader[UpdatedAt] = (cma: CmaCursor) =>
      parseTime(
        Option(cma.entry.getSystem.getUpdatedAt),
        "UpdateAt",
        time =>
          new UpdatedAt {
            val date: ZonedDateTime = time
          }
      )
  }

  trait FirstPublishedAt {
    val date: ZonedDateTime
    override def toString: String = date.toString
  }
  object FirstPublishedAt {
    implicit val reader: CmaReader[FirstPublishedAt] = (cma: CmaCursor) =>
      parseTime(
        Option(cma.entry.getSystem.getFirstPublishedAt),
        "FirstPublishedAt",
        time =>
          new FirstPublishedAt {
            val date: ZonedDateTime = time
          }
      )
  }

  trait PublishedAt {
    val date: ZonedDateTime
    override def toString: String = date.toString
  }
  object PublishedAt {
    implicit val reader: CmaReader[PublishedAt] = (cma: CmaCursor) =>
      parseTime(
        Option(cma.entry.getSystem.getPublishedAt),
        "PublishedAt",
        time =>
          new PublishedAt {
            val date: ZonedDateTime = time
          }
      )
  }
}
