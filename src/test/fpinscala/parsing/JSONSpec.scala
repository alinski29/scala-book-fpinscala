package fpinscala.parsing

import fpinscala.TestSpec
import fpinscala.parsing.JSON.*

class JSONSpec extends TestSpec:

  "JSON Parser" should {

    import fpinscala.parsing.MyParser.run
    val jsParser = JSON.parse(MyParser)

    "parse valid JSON" in {
      val jsonTxt = """
      {
        "Company name" : "Microsoft Corporation",
        "Ticker": "MSFT",
        "Active": true,
        "Price": 30.66,
        "Shares outstanding": 8.38e9,
        "Related companies": [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
      }
      """

      val maybeJs = jsParser.run(jsonTxt)
      maybeJs.isRight shouldBe true
      val js: Map[String, JSON] = maybeJs.toOption.get.asInstanceOf[JObject].get

      js("Company name") shouldEqual JString("Microsoft Corporation")
      js("Ticker") shouldEqual JString("MSFT")
      js("Price") shouldEqual JNumber(30.66)
      js("Related companies") shouldEqual
        JArray(Vector("HPQ", "IBM", "YHOO", "DELL", "GOOG").map(JString.apply))
    }

    "fail to parse invalid JSON" in {
      val malformedJson1 = """
        {
          "Company name" ; "Microsoft Corporation"
        }
        """

      jsParser.run(malformedJson1).isRight shouldBe false

      val malformedJson2 = """
      [
        [ "HPQ", "IBM",
        "YHOO", "DELL" ++
        "GOOG"
        ]
      ]
      """

      jsParser.run(malformedJson2).isRight shouldBe false
    }

  }
