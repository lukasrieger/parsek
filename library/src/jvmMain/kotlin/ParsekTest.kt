import stream.Stream
import stream.instances.StringStream

data class St(val callCount: Int)

val t1: ParserE<StringStream, Throwable, String> = pure("Hello!")

val tt3 = t1 or t1

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


fun main() {
    val result =
        ttt65.runParser(Stream.of("abc Hello World!"))
    println(result)
}
