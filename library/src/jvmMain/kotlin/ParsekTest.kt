import arrow.core.Either
import error.ParseErrorBundle
import stream.Stream
import stream.instances.StringStream

//
//data class St(val callCount: Int)
//
//val t1: ParserE<StringStream, Throwable, String> = pure("Hello!")
//
//val tt3 = t1 or t1
//
//val ttt6 = pure<StringStream, Int, Nothing, Int>(0)
//
//val xhguhr =  ttt6 * tt3
//
//var i = 0
//
//val t3: Parser<StringStream, String> = doM { "1" }
//
//fun t4(): ParsekT<StringStream, Any, Any, String> = doM {
//    val x = !t1
//
//    "$x -> $x"
//}
//
//
//val consumeA = char<String>('a') or char('b')
//
//val orTest: Parser<StringStream, String> = pure<StringStream, Any, Nothing, _>("ab") or pure("ac")
//
//val apTest = consumeA * orTest
//
//val apTest2 =
//    (consumeA) map { it }
//
//val ttt65: Parser<StringStream, Unit> = doM {
//    !char<String>('a')
//    !char<String>('b')
//    !char<String>('c')
//    !(char<String>('b') or char('a') or eof())
//}
//
//fun tRec1(): ParserE<StringStream, Throwable, String> = doM {
//    i += 1
//    println("Recursive and safe 1  $i")
//    !tRec2()
//}
//
//fun tRec2() = doM {
//    println("Recursive and safe 2")
//    !tRec1()
//}

val t1: ParserE<StringStream, Throwable, String> = pure("Hello!")

object Grammar {

    val comma = char(',')
    val colon = char(':').lexeme()

    val openingBrace = char('{').lexeme()
    val closingBrace = char('}').lexeme()

    val openingBracket = char('[').lexeme()
    val closingBracket = char(']').lexeme()

    val `null` = string("null").lexeme()
    val `true` = string("true").lexeme()
    val `false` = string("false").lexeme()

    val jsonNull = `null`.map { null as Any? }
    val jsonBool = `true`.map { true } or (`false`.map { false })
    val jsonString =
        (-char<String>('"') * anyChar<String>().manyTill(char('"'))).map { str -> str.joinToString("") }.lexeme()

    val jsonNumber = double()
    val jsonPrimitiveValue = (jsonString or jsonNull or jsonBool or jsonNumber).attempt()

    val jsonObject: ParsekT<Stream<String, Char>, Any, Nothing, Map<String, Any?>> =
        ((jsonString * -colon * ref(::root))
            .sepBy(comma.label("object separator")))
            .between(open = openingBrace, close = closingBrace).label("json object between")
            .map { it.toMap() }
            .label("in jsonObject")

    val jsonArray: ParsekT<Stream<String, Char>, Any, Nothing, List<Any?>> by lazy {
        (ref(::root)
            .sepBy(comma.label("separatorArray")))
            .between(open = openingBracket, close = closingBracket)
            .label("in root parser")
    }

    val root: Parser<Stream<String, Char>, Any?> = jsonPrimitiveValue or ref(::jsonObject) or ref(::jsonArray)


//    val seqTest =
//        jsonNull * space1() * jsonBool * space1() * jsonNumber * space1() * jsonString * eof()
}


fun main() {

    val results = mutableListOf<Either<ParseErrorBundle<StringStream, Nothing>, Any?>>()

    for(i in 1..10) {
        results += Grammar.root.runParser(Stream.of(jsonSample1K))
    }

    if (results.all { it is Either.Right }) println("yay") else println("ohno")

}


