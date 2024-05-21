internal sealed interface Trampoline<S : Stream<*, *>, out Error, out Context, out Output> {
    data class More<S : Stream<*, *>, Error, Context, Output>(
        val run: () -> Trampoline<S, Error, Context, Output>
    ) : Trampoline<S, Error, Context, Output>

    data class Done<S : Stream<*, *>, Error, Context, Output>(
        val done: Reply<S, Error, Context, Output>
    ) : Trampoline<S, Error, Context, Output>


    companion object {
        internal fun <S : Stream<*, *>, Error, Context, Output> done(
            reply: Reply<S, Error, Context, Output>
        ): Trampoline<S, Error, Context, Output> = Done(reply)

        internal fun <S : Stream<*, *>, Error, Context, Output> more(
            run: () -> Trampoline<S, Error, Context, Output>
        ): Trampoline<S, Error, Context, Output> = More(run)
    }
}

internal sealed interface Step<out Loop, out Result> {
    data class Done<Result>(val result: Result) : Step<Nothing, Result>
    data class Loop<Loop>(val loop: Loop) : Step<Loop, Nothing>
}