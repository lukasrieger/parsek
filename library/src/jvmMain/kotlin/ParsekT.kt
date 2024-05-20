import RunParser.Companion.done
import RunParser.Companion.more
import arrow.core.nonEmptyListOf
import arrow.core.toNonEmptyListOrNull
import stream.take1
import stream.takeN

interface ParsekT<out Error, out Context, out Output> {

    context(Context)
    fun <B> unparser(
        state: State<@UnsafeVariance Error>,
        trampoline: (() -> B) -> B,
        consumedOk: context(Context) (Output, State<Error>, Hints<Char>) -> B,
        consumedError: context(Context) (ParseError<Error>, State<Error>) -> B,
        emptyOk: context(Context) (Output, State<Error>, Hints<Char>) -> B,
        emptyError: context(Context) (ParseError<Error>, State<Error>) -> B
    ): B
}


internal sealed interface RunParser<out Error, out Context, out Output> {
    data class More<Error, Context, Output>(
        val run: () -> RunParser<Error, Context, Output>
    ) : RunParser<Error, Context, Output>

    data class Done<Error, Context, Output>(
        val done: Reply<Error, Output>
    ) : RunParser<Error, Context, Output>


    companion object {
        internal fun <Error, Context, Output> done(
            reply: Reply<Error, Output>
        ): RunParser<Error, Context, Output> = Done(reply)

        internal fun <Error, Context, Output> more(
            run: () -> RunParser<Error, Context, Output>
        ): RunParser<Error, Context, Output> = More(run)
    }
}

typealias ConsumedOk<E, C, O, B> = context(C) (O, State<E>, Hints<Char>) -> B
typealias ConsumedError<E, C, B> = context(C) (ParseError<E>, State<E>) -> B
typealias EmptyOk<E, C, O, B> = context(C) (O, State<E>, Hints<Char>) -> B
typealias EmptyError<E, C, B> = context(C) (ParseError<E>, State<E>) -> B


fun <Error, Context, Output> fail(message: String): ParsekT<Error, Context, Output> =
    object : ParsekT<Error, Context, Output> {
        context(Context) override fun <B> unparser(
            state: State<Error>,
            trampoline: (() -> B) -> B,
            consumedOk: ConsumedOk<Error, Context, Output, B>,
            consumedError: ConsumedError<Error, Context, B>,
            emptyOk: EmptyOk<Error, Context, Output, B>,
            emptyError: EmptyError<Error, Context, B>
        ): B = emptyError(
            this@Context,
            ParseError.FancyError(state.stateOffset, setOf(ErrorFancy.ErrorFail(message))),
            state
        )
    }

fun <Error, Context, Output> ParsekT<Error, Context, Output>.attempt() =
    object : ParsekT<Error, Context, Output> {
        context(Context) override fun <B> unparser(
            state: State<Error>,
            trampoline: (() -> B) -> B,
            consumedOk: ConsumedOk<Error, Context, Output, B>,
            consumedError: ConsumedError<Error, Context, B>,
            emptyOk: EmptyOk<Error, Context, Output, B>,
            emptyError: EmptyError<Error, Context, B>
        ): B = trampoline {
            this@attempt.unparser(
                state,
                trampoline,
                consumedOk,
                { err, _ -> emptyError(this@Context, err, state) },
                emptyOk,
                { err, _ -> emptyError(this@Context, err, state) }
            )
        }
    }


fun <Error, Context, Output> token(
    test: (Char) -> Output?,
    errorItems: Set<ErrorItem<Char>>
): ParsekT<Error, Context, Output> = object : ParsekT<Error, Context, Output> {
    context(Context) override fun <B> unparser(
        state: State<Error>,
        trampoline: (() -> B) -> B,
        consumedOk: ConsumedOk<Error, Context, Output, B>,
        consumedError: ConsumedError<Error, Context, B>,
        emptyOk: EmptyOk<Error, Context, Output, B>,
        emptyError: EmptyError<Error, Context, B>
    ): B = when (val taken = state.stateInput.take1()) {
        null ->
            emptyError(
                this@Context,
                ParseError.TrivialError(state.stateOffset, ErrorItem.EndOfInput, errorItems),
                state
            )

        else -> {
            val (current, tail) = taken

            when (val res = test(current)) {
                null ->
                    emptyError(
                        this@Context,
                        ParseError.TrivialError(
                            state.stateOffset,
                            ErrorItem.Tokens(nonEmptyListOf(current)),
                            errorItems
                        ),
                        State(state.stateInput, state.stateOffset, state.statePosState, state.stateParseErrors)
                    )

                else ->
                    consumedOk(
                        this@Context,
                        res,
                        State(tail, state.stateOffset + 1, state.statePosState, state.stateParseErrors),
                        Hints.empty()
                    )
            }
        }
    }
}


fun <Error, Context> tokens(
    test: (String, String) -> Boolean,
    tokens: String
) = object : ParsekT<Error, Context, String> {
    context(Context) override fun <B> unparser(
        state: State<Error>,
        trampoline: (() -> B) -> B,
        consumedOk: ConsumedOk<Error, Context, String, B>,
        consumedError: ConsumedError<Error, Context, B>,
        emptyOk: EmptyOk<Error, Context, String, B>,
        emptyError: EmptyError<Error, Context, B>
    ): B {
        val unexpected: (Int, ErrorItem<Char>) -> ParseError<Error> = { pos, u ->
            ParseError.TrivialError(
                pos, u, setOf(ErrorItem.Tokens(tokens.toList().toNonEmptyListOrNull()!!))
            )
        }

        return when (val taken = state.stateInput.takeN()) {
            null ->
                emptyError(this@Context, unexpected(state.stateOffset, ErrorItem.EndOfInput), state)

            else -> {
                val (tts, tail) = taken

                if (test(tokens, tts)) {
                    val nextState =
                        State(
                            tail,
                            state.stateOffset + tokens.length,
                            state.statePosState,
                            state.stateParseErrors
                        )

                    if (tokens.isEmpty()) {
                        emptyOk(this@Context, tts, nextState, Hints.empty())
                    } else {
                        consumedOk(this@Context, tts, nextState, Hints.empty())
                    }
                } else {
                    emptyError(
                        this@Context,
                        unexpected(state.stateOffset, ErrorItem.Tokens(tts.toList().toNonEmptyListOrNull()!!)),
                        State(state.stateInput, state.stateOffset, state.statePosState, state.stateParseErrors)
                    )
                }
            }
        }
    }
}


fun <Error, Context, Output> pure(pure: Output) =
    object : ParsekT<Error, Context, Output> {
        context(Context)
        override fun <B> unparser(
            state: State<Error>,
            trampoline: (() -> B) -> B,
            consumedOk: ConsumedOk<Error, Context, Output, B>,
            consumedError: ConsumedError<Error, Context, B>,
            emptyOk: EmptyOk<Error, Context, Output, B>,
            emptyError: EmptyError<Error, Context, B>
        ): B = trampoline { emptyOk(this@Context, pure, state, Hints.empty()) }
    }


fun <Error, Context, Output1, Output2> ParsekT<Error, Context, Output1>.bind(
    cont: (Output1) -> ParsekT<Error, Context, Output2>
): ParsekT<Error, Context, Output2> = object : ParsekT<Error, Context, Output2> {
    context(Context) override fun <B> unparser(
        state: State<@UnsafeVariance Error>,
        trampoline: (() -> B) -> B,
        consumedOk: context(Context) (Output2, State<Error>, Hints<Char>) -> B,
        consumedError: context(Context) (ParseError<Error>, State<Error>) -> B,
        emptyOk: context(Context) (Output2, State<Error>, Hints<Char>) -> B,
        emptyError: context(Context) (ParseError<Error>, State<Error>) -> B
    ): B = trampoline {
        this@bind.unparser(
            state,
            trampoline,
            { a, b, c ->
                trampoline {
                    cont(a).unparser(
                        b,
                        trampoline,
                        consumedOk,
                        consumedError,
                        emptyOk,
                        emptyError
                    )
                }
            },
            consumedError,
            { a, b, c ->
                trampoline {
                    cont(a).unparser(
                        b,
                        trampoline,
                        consumedOk,
                        consumedError,
                        emptyOk,
                        emptyError
                    )
                }
            },
            emptyError
        )
    }
}


sealed interface Step<out A, out B> {
    data class Done<B>(val b: B) : Step<Nothing, B>
    data class Loop<A>(val a: A) : Step<A, Nothing>
}


internal fun <E, C, O> tailRec(
    next: (() -> RunParser<E, C, O>) -> Step<() -> RunParser<E, C, O>, Reply<E, O>>,
    initial: () -> RunParser<E, C, O>
): Reply<E, O> {
    tailrec fun loop(arg: () -> RunParser<E, C, O>): Reply<E, O> =
        when (val k = next(arg)) {
            is Step.Done -> k.b
            is Step.Loop -> loop(k.a)
        }

    return loop(initial)
}


fun <Error, Context, Output> runParsekT(
    parser: ParsekT<Error, Context, Output>,
    initialState: State<Error>,
    context: Context
): Reply<Error, Output> {
    tailrec fun go(
        n: () -> RunParser<Error, Context, Output>
    ): Step<() -> RunParser<Error, Context, Output>, Reply<Error, Output>> = when (val step = n()) {
        is RunParser.Done -> Step.Done(step.done)
        is RunParser.More -> go(step.run)
    }

    return tailRec(::go) {
        context.run {
            parser.unparser(
                state = initialState,
                trampoline = ::more,
                consumedOk = { a, s, hs ->
                    done(Reply(s, Consumption.Consumed, Result.Ok(hs, a)))
                },
                consumedError = { err, s ->
                    done(Reply(s, Consumption.Consumed, Result.Error(err)))
                },
                emptyOk = { a, s, hs ->
                    done(Reply(s, Consumption.NotConsumed, Result.Ok(hs, a)))
                },
                emptyError = { err, s ->
                    done(Reply(s, Consumption.NotConsumed, Result.Error(err)))
                }
            )
        }
    }
}
