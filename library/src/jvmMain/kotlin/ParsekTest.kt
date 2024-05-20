data class St(val callCount: Int)

val t1: ParsekT<Nothing, Nothing, String> = pure("Hello!")

val t2: ParsekT<Nothing, Nothing, Int> = pure(0)

var i = 0
val t3: ParsekT<Nothing, St, String>
    get() = doM {
        !t1

        updateContext {
            it.copy(callCount = it.callCount + 1)
        }

        val currentContext = context()

        println(currentContext.callCount)

        !t3
        "1"
    }

fun t4() = doM {
    val x = !t1
    val y = !t2

    "$x -> $y"
}



fun tRec1(): ParsekT<Nothing, St, String> = doM {
    val x = !t1

    i += 1
    println("Recursive and safe 1  $i")
    !tRec2()
}

fun tRec2(): ParsekT<Nothing, St, String> = doM {
    val x = !t1

    println("Recursive and safe 2")
    !tRec1()
}


fun main() {
    println(t4().runParsekT("emptyPath", "Hello World!"))
}
