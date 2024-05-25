import arrow.core.Either
import stream.Stream
import stream.instances.StringStream

data class St(val callCount: Int)

val t1: ParserE<StringStream, Throwable, String> = pure("Hello!")

val tt3 = t1 or t1

val ttt6 = pure<StringStream, Int, Nothing, Int>(0)

val xhguhr =  ttt6 * tt3

var i = 0

val t3: Parser<StringStream, String> = doM { "1" }

fun t4(): ParsekT<StringStream, Any, Any, String> = doM {
    val x = !t1

    "$x -> $x"
}


val consumeA = char<String>('a') or char('b')

val orTest: Parser<StringStream, String> = pure<StringStream, Any, Nothing, _>("ab") or pure("ac")

val apTest = consumeA * orTest

val apTest2 =
    (consumeA) map { it }

val ttt65: Parser<StringStream, Unit> = doM {
    !char<String>('a')
    !char<String>('b')
    !char<String>('c')
    !(char<String>('b') or char('a') or eof())
}

fun tRec1(): ParserE<StringStream, Throwable, String> = doM {
    i += 1
    println("Recursive and safe 1  $i")
    !tRec2()
}

fun tRec2() = doM {
    println("Recursive and safe 2")
    !tRec1()
}


val comma = char(',')
val colon = char(':')

val openingBrace = char('{')
val closingBrace = char('}')

val openingBracket = char('[')
val closingBracket = char(']')

val `null` = string("null")
val `true` = string("true")
val `false` = string("false")

val jsonNull = `null` map { null }
val jsonBool = `true`.map { true } or (`false`.map { false })
val jsonString =
    (-char<String>('"') * anyChar<String>().manyTill(char('"'))) map { str -> str.joinToString("") }

val jsonNumber = double()
val jsonPrimitiveValue = jsonNull or jsonBool or jsonString or (jsonNumber)

val seqTest =
    jsonNull * space1() * jsonBool * space1() * jsonNumber * space1() * jsonString * eof()


fun main() {
    val result =
        seqTest.runParser(
            Stream.of("""
                |null 
                |false 
                |1.53754 <boom!>
                |"Hello World!"
                |""".trimMargin()
            )
        )

    when(result) {
        is Either.Left -> println(result.value)
        is Either.Right -> println(result.value)
    }
}


