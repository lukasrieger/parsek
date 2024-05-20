import kotlinx.coroutines.suspendCancellableCoroutine
import kotlinx.io.files.Path
import kotlin.Result
import kotlin.coroutines.*
import kotlin.experimental.ExperimentalTypeInference
import kotlin.reflect.KProperty0


class ParsekScope<Error, Context, O> : Continuation<ParsekT<Error, Context, O>> {

    override val context: CoroutineContext = EmptyCoroutineContext
    override fun resumeWith(result: Result<ParsekT<Error, Context, O>>) {
        result.fold(
            onSuccess = { returnedParsekT = it },
            onFailure = { throw it }
        )
    }

    suspend fun context(): @UnsafeVariance Context = bind(object : ParsekT<Error, Context, Context> {
        context(Context) override fun <B> unparser(
            state: State<Error>,
            trampoline: (() -> B) -> B,
            consumedOk: ConsumedOk<Error, Context, Context, B>,
            consumedError: ConsumedError<Error, Context, B>,
            emptyOk: EmptyOk<Error, Context, Context, B>,
            emptyError: EmptyError<Error, Context, B>
        ): B = emptyOk(this@Context, this@Context, state, Hints.empty())
    })

    private lateinit var returnedParsekT: ParsekT<Error, Context, O>

    fun returnedMonad(): ParsekT<Error, Context, O> = returnedParsekT

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
fun <E, C, O> doM(@BuilderInference f: suspend ParsekScope<E, C, O>.(C) -> O): ParsekT<E, C, O> {
    val withContext: suspend ParsekScope<E, C, O>.() -> O = { f(context()) }
    val scope = ParsekScope<E, C, O>()
    val wrapReturn: suspend ParsekScope<E, C, O>.() -> ParsekT<E, C, O> = { pure(withContext()) }
    wrapReturn.startCoroutine(scope, scope)

    return scope.returnedMonad()
}

class St(var callCount: Int)

val t1: ParsekT<Nothing, St, String> = pure("Hello!")

val t2: ParsekT<Nothing, Nothing, Int> = pure(0)

var i = 0
val t3: ParsekT<Nothing, St, String>
    get() = doM {
        println(i)
        !t1
        i += 1

        println(it.callCount)
        "1"
    }

fun t4() = doM { cp: St ->
    val x = !t1
    val y = !t2

    cp.callCount

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
    St(0).run {

        println(
            runParsekT(
                tRec1(),
                State(
                    stateInput = "",
                    stateOffset = 0,
                    statePosState = PosState(
                        pStateLinePrefix = "",
                        pStateOffset = 0,
                        pStateInput = "",
                        pStateTabWidth = Pos(4),
                        pStateSourcePos = SourcePos(
                            sourceColumn = Pos(0),
                            sourceLine = Pos(0),
                            sourceName = Path("")
                        )
                    ),
                    stateParseErrors = emptyList()
                ),
                St(0)
            ).result
        )
    }

}
