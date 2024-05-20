import kotlinx.io.files.Path
import kotlin.Result
import kotlin.coroutines.*
import kotlin.experimental.ExperimentalTypeInference
import kotlin.reflect.KProperty0
import updateContext as _updateContext


class ParsekScope<Error, Context, O> : Continuation<ParsekT<Error, @UnsafeVariance Context, O>> {

    override val context: CoroutineContext = EmptyCoroutineContext
    override fun resumeWith(result: Result<ParsekT<Error, Context, O>>) {
        result.fold(
            onSuccess = { returnedParsekT = it },
            onFailure = { throw it }
        )
    }
    private lateinit var returnedParsekT: ParsekT<Error, Context, O>

    fun returnedMonad(): ParsekT<Error, Context, O> = returnedParsekT


    suspend fun context(): Context = !getContext<Context>()

    suspend fun updateContext(fn: (Context) -> Context): Unit = !_updateContext(fn)

    private suspend fun <B> bind(m: ParsekT<Error, Context, B>): B = suspendCoroutine { cont ->
        returnedParsekT = m.bind {
            cont.resumeWith(Result.success(it))
            returnedParsekT
        }
    }

    suspend operator fun <R> ParsekT<Error, Context, R>.not(): R = bind(this)
}

@OptIn(ExperimentalTypeInference::class)
@BuilderInference
fun <E, C, O> doM(@BuilderInference f: suspend ParsekScope<E, C, O>.() -> O): ParsekT<E, C, O> {
    val scope = ParsekScope<E, C, O>()
    val wrapReturn: suspend ParsekScope<E, C, O>.() -> ParsekT<E, C, O> = { pure(f()) }
    wrapReturn.startCoroutine(scope, scope)

    return scope.returnedMonad()
}

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


fun <E, C, O> ref(
    parserProperty: KProperty0<ParsekT<E, C, O>>
): ParsekT<E, C, O> {
    return parserProperty()
}


fun main() {
    println(t4().runParsekT("emptyPath", "Hello World!"))
}
