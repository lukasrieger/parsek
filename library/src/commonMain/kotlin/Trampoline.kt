
internal sealed interface Trampoline<out Error, out Context, out Output> {
    data class More<Error, Context, Output>(
        val run: () -> Trampoline<Error, Context, Output>
    ) : Trampoline<Error, Context, Output>

    data class Done<Error, Context, Output>(
        val done: Reply<Error, Context, Output>
    ) : Trampoline<Error, Context, Output>


    companion object {
        internal fun <Error, Context, Output> done(
            reply: Reply<Error, Context, Output>
        ): Trampoline<Error, Context, Output> = Done(reply)

        internal fun <Error, Context, Output> more(
            run: () -> Trampoline<Error, Context, Output>
        ): Trampoline<Error, Context, Output> = More(run)
    }
}

internal sealed interface Step<out Loop, out Result> {
    data class Done<Result>(val result: Result) : Step<Nothing, Result>
    data class Loop<Loop>(val loop: Loop) : Step<Loop, Nothing>
}