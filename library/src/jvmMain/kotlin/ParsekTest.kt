data class St(val callCount: Int)

val t1= pure("Hello!")

val t2 = pure(0) map { "$it" }

val tt3 = t1 or t2

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


fun tRec1() = doM {
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
    println(t4().runParsekT( "Hello World!"))
}
