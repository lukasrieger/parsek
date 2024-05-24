import stream.Stream
import stream.instances.StringStream
import kotlin.Result
import kotlin.coroutines.*
import updateContext as _updateContext

class ParsekScope<S : Stream<*, *>, Context, Error, Output> : Continuation<ParsekT<S, Context, Error, Output>> {

    override val context: CoroutineContext = EmptyCoroutineContext
    override fun resumeWith(result: Result<ParsekT<S, Context, Error, Output>>) {
        result.fold(
            onSuccess = { returnedParsekT = it },
            onFailure = { throw it }
        )
    }

    private lateinit var returnedParsekT: ParsekT<S, Context, Error, Output>

    internal fun getParser(): ParsekT<S, Context, Error, Output> = returnedParsekT

    suspend fun updateContext(fn: (Context) -> Context): Unit = bind(_updateContext(fn))

    suspend fun context(): Context = bind(getContext())

    private suspend fun <B> bind(m: ParsekT<S, Context, Error, B>): B = suspendCoroutine { cont ->
        returnedParsekT = m.bind {
            cont.resumeWith(Result.success(it))
            returnedParsekT
        }
    }

    suspend operator fun <R> ParsekT<S, Context, Error, R>.not(): R = bind(this)
}

fun <Error, Context : Any, Output> doM(
    f: suspend ParsekScope<StringStream, Context, Error, Output>.() -> Output
): ParsekT<StringStream, Context, Error, Output> {
    val scope = ParsekScope<StringStream, Context, Error, Output>()
    val wrapReturn: suspend ParsekScope<StringStream, Context, Error, Output>.() -> ParsekT<StringStream, Context, Error, Output> =
        {
            pure(f())
        }
    wrapReturn.startCoroutine(scope, scope)

    return scope.getParser()
}