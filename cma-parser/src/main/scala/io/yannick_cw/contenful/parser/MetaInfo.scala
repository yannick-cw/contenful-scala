package io.yannick_cw.contenful.parser

import com.contentful.java.cma.model.CMAEntry

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

  trait CreatedAt { val date: String }
  object CreatedAt {
    implicit val createdReader: CmaReader[CreatedAt] = (cma: CmaCursor) =>
      Option(cma.entry.getSystem.getCreatedAt)
        .map(
          x =>
            new CreatedAt {
              val date: String = x
          }
        )
        .toRight("Did not find a creation time")
  }

  trait UpdatedAt { val date: String }
  object UpdatedAt {
    implicit val reader: CmaReader[UpdatedAt] = (cma: CmaCursor) =>
      Option(cma.entry.getSystem.getUpdatedAt)
        .map(
          x =>
            new UpdatedAt {
              val date: String = x
          }
        )
        .toRight("Did not find a updated time")
  }

  trait FirstPublishedAt { val date: String }
  object FirstPublishedAt {
    implicit val reader: CmaReader[FirstPublishedAt] = (cma: CmaCursor) =>
      Option(cma.entry.getSystem.getFirstPublishedAt)
        .map(
          x =>
            new FirstPublishedAt {
              val date: String = x
          }
        )
        .toRight("Did not find a first published time")
  }

  trait PublishedAt { val date: String }
  object PublishedAt {
    implicit val reader: CmaReader[PublishedAt] = (cma: CmaCursor) =>
      Option(cma.entry.getSystem.getPublishedAt)
        .map(
          x =>
            new PublishedAt {
              val date: String = x
          }
        )
        .toRight("Did not find a published time")
  }
}
