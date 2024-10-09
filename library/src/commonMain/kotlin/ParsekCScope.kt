import kotlin.Result
import kotlin.coroutines.Continuation
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext
import kotlin.coroutines.startCoroutine
import kotlin.coroutines.suspendCoroutine


class ParsekCScope<R, E, O> : Continuation<ParsekC<R, E, O>> {
    override val context: CoroutineContext = EmptyCoroutineContext

    override fun resumeWith(result: Result<ParsekC<R, E, O>>) {
        result.fold(
            onSuccess = { returnedParsekT = it },
            onFailure = { throw it }
        )
    }

    private lateinit var returnedParsekT: ParsekC<R, E, O>

    internal fun getParser(): ParsekC<R, E, O> = returnedParsekT

    suspend fun updateEnvironment(fn: (R) -> R): Unit = bind(modifyEnvironment(fn))

    suspend fun environment(): R = bind(environment<R>())

    private suspend fun <B> bind(m: ParsekC<R, E, B>): B = suspendCoroutine { cont ->
        returnedParsekT = m.bind {
            cont.resumeWith(Result.success(it))
            returnedParsekT
        }
    }

    suspend operator fun <B> ParsekC<R, E, B>.not(): B = bind(this)
}

fun <R, E, O> doParser(
    f: suspend ParsekCScope<R, E, O>.() -> O
): ParsekC<R, E, O> {
    val scope = ParsekCScope<R, E, O>()
    val wrapReturn: suspend ParsekCScope<R, E, O>.() -> ParsekC<R, E, O> =
        {
            pureCC(f())
        }
    wrapReturn.startCoroutine(scope, scope)

    return scope.getParser()
}