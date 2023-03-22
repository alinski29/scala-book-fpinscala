package fpinscala.parsing

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  def parse[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*

    def token(s: String) = string(s).token

    def array: Parser[JSON] = (
      token("[") *> value.sep(token(",")).map(vs => JArray(vs.toIndexedSeq)) <* token("]")
    ).scope("array")

    def obj: Parser[JSON] = (
      token("{") *> keyval.sep(token(",")).map(kvs => JObject(kvs.toMap)) <* token("}")
    ).scope("object")

    def keyval: Parser[(String, JSON)] = (escapedQuoted <* token(":")) ** value

    def lit: Parser[JSON] = (
      token("null").map(_ => JNull) |
      double.map(n => JNumber(n)) |
      escapedQuoted.map(JString(_)) |
      token("true").map(_ => JBool(true)) |
      token("false").map(_ => JBool(false))
    ).scope("literal")

    def value: Parser[JSON] = lit | obj | array

    (whitespace *> (obj | array)).root

