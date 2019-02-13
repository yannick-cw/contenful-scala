# contentful-scala

# Generic CMAEntry reader

```scala
import com.contentful.java.cma.model.CMAEntry
import io.yannick_cw.contenful.parser.MetaInfo._
import io.yannick_cw.contenful.parser.syntax._
import io.yannick_cw.contenful.parser.auto._

case class Person(
  name: String,
  age: Int,
  more: Option[String],
  results: List[Int],
  // meta-info
  createdAt: CreatedAt,
  updatedAt: Option[UpdatedAt],
  firstPublishedAt: Option[FirstPublishedAt],
  publishedAt: Option[PublishedAt],
  status: Status
)

val entry: CmaEntry = ???
println(entry.as[Person])
```
