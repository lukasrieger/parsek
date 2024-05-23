data class St(val callCount: Int)

val t1: ParserE<StringStream, Throwable, String> = string("Hello!")

val tt3 = t1 or t1

var i = 0

val t3: Parser<StringStream, String> = doM { "1" }

fun t4(): ParsekT<StringStream, Any, Any, String> = doM {
    val x = !t1

    "$x -> $x"
}


val consumeA = char<String>('a') or char('b')

val orTest = string("ab") or string("ac")

val ttt65: ParsekT<StringStream, Any, Int, String> = doM {
    !string("Hello World!")

    updateContext {
        val x: Number = 0

        0
    }


    val x = !getContext<StringStream, Int>()

    val y = !consumeA

    "TODO()"
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
        consumeA.runParser(Stream.of("cab Hello World!"))
    println(result)
}
