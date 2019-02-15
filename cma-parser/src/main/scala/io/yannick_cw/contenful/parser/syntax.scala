package io.yannick_cw.contenful.parser

import com.contentful.java.cma.model.CMAEntry
import io.yannick_cw.contenful.parser.CmaReader.Result

object syntax {
  implicit class ReadCma(cma: CMAEntry) {
    def as[A: CmaReader]: Result[A] =
      CmaReader[A].read(CmaCursor(None, cma))
  }
}
