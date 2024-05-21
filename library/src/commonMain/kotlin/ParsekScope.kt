import kotlin.Result
import kotlin.coroutines.*
import updateContext as _updateContext

class ParsekScope<S : Stream<*, *>, Error, Context, Output> : Continuation<ParsekT<S, Error, Context, Output>> {

    override val context: CoroutineContext = EmptyCoroutineContext
    override fun resumeWith(result: Result<ParsekT<S, Error, Context, Output>>) {
        result.fold(
            onSuccess = { returnedParsekT = it },
            onFailure = { throw it }
        )
    }

    private lateinit var returnedParsekT: ParsekT<S, Error, Context, Output>

    internal fun getParser(): ParsekT<S, Error, Context, Output> = returnedParsekT

    suspend fun updateContext(fn: (Context) -> Context): Unit = bind(_updateContext(fn))

    suspend fun context(): Context = bind(getContext())

    private suspend fun <B> bind(m: ParsekT<S, Error, Context, B>): B = suspendCoroutine { cont ->
        returnedParsekT = m.bind {
            cont.resumeWith(Result.success(it))
            returnedParsekT
        }
    }

    suspend operator fun <R> ParsekT<S, Error, Context, R>.not(): R = bind(this)
}

fun <Error : Any, Context : Any, Output> doM(
    f: suspend ParsekScope<StringStream, Error, Context, Output>.() -> Output
): ParsekT<StringStream, Error, Context, Output> {
    val scope = ParsekScope<StringStream, Error, Context, Output>()
    val wrapReturn: suspend ParsekScope<StringStream, Error, Context, Output>.() -> ParsekT<StringStream, Error, Context, Output> =
        {
            pure(f())
        }
    wrapReturn.startCoroutine(scope, scope)

    return scope.getParser()
}