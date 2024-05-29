import stream.Stream

sealed interface Trampoline<in S : Stream<*, *>, in Context, out Error, out Output> {
    data class More<S : Stream<*, *>, Context, Error, Output>(
        val run: () -> Trampoline<S, Context, Error, Output>
    ) : Trampoline<S, Context, Error, Output>

    data class Done<S : Stream<*, *>, Context, Error, Output>(
        val done: Reply<S, Context, Error, Output>
    ) : Trampoline<S, Context, Error, Output>


    companion object {
        internal fun <S : Stream<*, *>, Context, Error, Output> done(
            reply: Reply<S, Context, Error, Output>
        ): Trampoline<S, Context, Error, Output> = Done(reply)

        internal fun <S : Stream<*, *>, Context, Error, Output> more(
            run: () -> Trampoline<S, Context, Error, Output>
        ): Trampoline<S, Context, Error, Output> = More(run)
    }
}

sealed interface Step<out Loop, out Result> {
    data class Done<Result>(val result: Result) : Step<Nothing, Result>
    data class Loop<Loop>(val loop: Loop) : Step<Loop, Nothing>
}