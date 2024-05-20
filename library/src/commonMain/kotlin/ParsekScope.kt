import kotlin.Result
import kotlin.coroutines.*
import updateContext as _updateContext

class ParsekScope<Error, Context, Output> : Continuation<ParsekT<Error, Context, Output>> {

    override val context: CoroutineContext = EmptyCoroutineContext
    override fun resumeWith(result: Result<ParsekT<Error, Context, Output>>) {
        result.fold(
            onSuccess = { returnedParsekT = it },
            onFailure = { throw it }
        )
    }

    private lateinit var returnedParsekT: ParsekT<Error, Context, Output>

    internal fun getParser(): ParsekT<Error, Context, Output> = returnedParsekT


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

fun <Error, Context, Output> doM(
    f: suspend ParsekScope<Error, Context, Output>.() -> Output
): ParsekT<Error, Context, Output> {
    val scope = ParsekScope<Error, Context, Output>()
    val wrapReturn: suspend ParsekScope<Error, Context, Output>.() -> ParsekT<Error, Context, Output> = { pure(f()) }
    wrapReturn.startCoroutine(scope, scope)

    return scope.getParser()
}