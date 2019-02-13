package io.yannick_cw.contenful.parser

import com.contentful.java.cma.model.CMAEntry

object syntax {
  implicit class ReadCma(cma: CMAEntry) {
    def as[A: CmaReader]: Either[String, A] =
      CmaReader[A].read(CmaCursor(None, cma))
  }
}
