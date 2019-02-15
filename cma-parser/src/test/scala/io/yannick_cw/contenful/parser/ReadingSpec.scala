package io.yannick_cw.contenful.parser

import java.time.ZonedDateTime
import java.util

import com.contentful.java.cma.model.{CMAEntry, CMALink}
import com.google.gson.internal.LinkedTreeMap
import io.yannick_cw.contenful.parser.MetaInfo.Coordinates
import io.yannick_cw.contenful.parser.auto._
import io.yannick_cw.contenful.parser.syntax._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{EitherValues, FunSuite, Matchers}

case class TestContent(
    text: String,
    intNum: Int,
    decNum: Double,
    opt: Option[String],
    list: List[String],
    dateField: ZonedDateTime,
    locField: Coordinates,
    mediaField: CMALink,
    boolField: Boolean
)

case class T[A](testValue: A)

class ReadingSpec
    extends FunSuite
    with Matchers
    with PropertyChecks
    with EitherValues {

  test("it should compile for all base types") {
    """
      |import io.yannick_cw.contenful.parser.syntax._
      |import io.yannick_cw.contenful.parser.auto._
      |
      |val entry: CMAEntry = ???
      |entry.as[TestContent]
    """.stripMargin should compile
  }

  test("testStringReader") {
    forAll(Gen.alphaStr) { testValue =>
      val entry = new CMAEntry().setField("testValue", "de-DE", testValue)
      entry.as[T[String]] shouldBe Right(T(testValue))
    }
  }

  test("testDoubleReader") {
    forAll(Gen.posNum[Double]) { testValue =>
      val entry = new CMAEntry().setField("testValue", "de-DE", testValue)
      entry.as[T[Double]] shouldBe Right(T(testValue))
    }
  }
  test("testIntReader") {
    forAll(Gen.negNum[Double]) { testValue =>
      val entry = new CMAEntry().setField("testValue", "de-DE", testValue)
      entry.as[T[Int]] shouldBe Right(T(testValue.toInt))
    }
  }
  test("testDateTimeReader") {
    forAll(Gen.alphaStr) { testValue =>
      val entry = new CMAEntry().setField("testValue", "de-DE", testValue)
      entry.as[T[String]] shouldBe Right(T(testValue))
    }
  }
  test("testCoordinatesReader") {
    forAll(Gen.posNum[Double], Gen.posNum[Double]) { (longitude, lattitude) =>
      val map = new LinkedTreeMap[String, Double]()
      map.put("lon", longitude)
      map.put("lat", lattitude)
      val entry = new CMAEntry().setField("testValue", "de-DE", map)
      entry.as[T[Coordinates]] shouldBe Right(T(new Coordinates {
        override val lon: Double = longitude
        override val lat: Double = lattitude
      }))
    }
  }

  val cmaLinkGen: Gen[LinkedTreeMap[String, LinkedTreeMap[String, String]]] =
    for {
      id    <- Gen.alphaStr
      _type <- Gen.oneOf("Entry", "Asset")
    } yield {
      val map      = new LinkedTreeMap[String, LinkedTreeMap[String, String]]()
      val innerMap = new LinkedTreeMap[String, String]()
      innerMap.put("linkType", _type)
      innerMap.put("id", id)
      map.put("sys", innerMap)
      map
    }
  test("testCmaLinkReader") {
    forAll(cmaLinkGen) { testValue =>
      val entry    = new CMAEntry().setField("testValue", "de-DE", testValue)
      val linkType = testValue.get("sys").get("linkType")
      val id       = testValue.get("sys").get("id")
      entry
        .as[T[CMALink]]
        .right
        .value
        .testValue
        .toString shouldBe s"CMALink { CMAResource { system = CMASystem { type = Link, linkType = $linkType, id = $id } } }"
    }
  }
  test("testBoolReader") {
    forAll(Gen.oneOf(true, false)) { testValue =>
      val entry = new CMAEntry().setField("testValue", "de-DE", testValue)
      entry.as[T[Boolean]] shouldBe Right(T(testValue))
    }
  }
  test("testOptReader") {
    forAll(Gen.option(Gen.alphaStr)) { testValue =>
      val entry = testValue.fold(new CMAEntry())(
        v => new CMAEntry().setField("testValue", "de-DE", v)
      )
      entry.as[T[Option[String]]] shouldBe Right(T(testValue))
    }
  }
  test("testStringListReader") {
    forAll(Gen.listOf(Gen.alphaStr)) { testValue =>
      val entry = new CMAEntry()
        .setField("testValue", "de-DE", util.Arrays.asList(testValue: _*))
      entry.as[T[List[String]]] shouldBe Right(T(testValue))
    }
  }
  test("testIntListReader") {
    forAll(Gen.listOf(Gen.posNum[Double])) { testValue =>
      val entry = new CMAEntry()
        .setField("testValue", "de-DE", util.Arrays.asList(testValue: _*))
      entry.as[T[List[Int]]] shouldBe Right(T(testValue.map(_.toInt)))
    }
  }
  test("testDoubleListReader") {
    forAll(Gen.listOf(Gen.negNum[Double])) { testValue =>
      val entry = new CMAEntry()
        .setField("testValue", "de-DE", util.Arrays.asList(testValue: _*))
      entry.as[T[List[Double]]] shouldBe Right(T(testValue))
    }
  }
  test("testCmaLinkListReader") {
    forAll(Gen.listOf(cmaLinkGen)) { testValue =>
      val entry = new CMAEntry()
        .setField("testValue", "de-DE", util.Arrays.asList(testValue: _*))
      entry
        .as[T[List[CMALink]]]
        .right
        .value
        .testValue
        .map(_.getId) should contain theSameElementsAs testValue.map(
        _.get("sys").get("id")
      )
    }
  }
  test("testBoolListReader") {
    forAll(Gen.listOf(Gen.oneOf(true, false))) { testValue =>
      val entry = new CMAEntry()
        .setField("testValue", "de-DE", util.Arrays.asList(testValue: _*))
      entry.as[T[List[Boolean]]] shouldBe Right(T(testValue))
    }
  }
}
