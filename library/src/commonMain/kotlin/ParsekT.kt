import Trampoline.Companion.done
import Trampoline.Companion.more
import arrow.core.nonEmptyListOf
import stream.take1
import stream.takeN
import util.toNonEmptyList


interface ParsekT<out Error, out Context, out Output> {
    fun <B> unparser(
        state: State<@UnsafeVariance Error, @UnsafeVariance Context>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output, State<Error, @UnsafeVariance Context>, Hints<Char>) -> B,
        consumedError: (ParseError<Error>, State<Error, @UnsafeVariance Context>) -> B,
        emptyOk: (Output, State<Error, @UnsafeVariance Context>, Hints<Char>) -> B,
        emptyError: (ParseError<Error>, State<Error, @UnsafeVariance Context>) -> B
    ): B
}


typealias ParserE<Error, Output> = ParsekT<Error, Nothing, Output>
typealias ParserC<Context, Output> = ParsekT<Nothing, Context, Output>
typealias Parser<Output> = ParsekT<Nothing, Nothing, Output>


internal typealias ConsumedOk<E, C, O, B> = (O, State<E, C>, Hints<Char>) -> B
internal typealias ConsumedError<E, C, B> = (ParseError<E>, State<E, C>) -> B
internal typealias EmptyOk<E, C, O, B> = (O, State<E, C>, Hints<Char>) -> B
internal typealias EmptyError<E, C, B> = (ParseError<E>, State<E, C>) -> B


fun <Error, Context, Output> fail(message: String): ParsekT<Error, Context, Output> =
    object : ParsekT<Error, Context, Output> {
        override fun <B> unparser(
            state: State<Error, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: ConsumedOk<Error, Context, Output, B>,
            consumedError: ConsumedError<Error, Context, B>,
            emptyOk: EmptyOk<Error, Context, Output, B>,
            emptyError: EmptyError<Error, Context, B>
        ): B = emptyError(
            ParseError.FancyError(state.stateOffset, setOf(ErrorFancy.ErrorFail(message))),
            state
        )
    }

fun <Error, Context, Output> ParsekT<Error, Context, Output>.attempt() =
    object : ParsekT<Error, Context, Output> {
        override fun <B> unparser(
            state: State<Error, Context>,
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
                { err, _ -> emptyError(err, state) },
                emptyOk,
                { err, _ -> emptyError(err, state) }
            )
        }
    }


fun <Error, Context, Output> token(
    test: (Char) -> Output?,
    errorItems: Set<ErrorItem<Char>>
): ParsekT<Error, Context, Output> = object : ParsekT<Error, Context, Output> {
    override fun <B> unparser(
        state: State<Error, Context>,
        trampoline: (() -> B) -> B,
        consumedOk: ConsumedOk<Error, Context, Output, B>,
        consumedError: ConsumedError<Error, Context, B>,
        emptyOk: EmptyOk<Error, Context, Output, B>,
        emptyError: EmptyError<Error, Context, B>
    ): B = when (val taken = state.stateInput.take1()) {
        null ->
            emptyError(
                ParseError.TrivialError(state.stateOffset, ErrorItem.EndOfInput, errorItems),
                state
            )

        else -> {
            val (current, tail) = taken

            when (val res = test(current)) {
                null ->
                    emptyError(
                        ParseError.TrivialError(
                            state.stateOffset,
                            ErrorItem.Tokens(nonEmptyListOf(current)),
                            errorItems
                        ),
                        State(
                            state.stateInput,
                            state.stateContext,
                            state.stateOffset,
                            state.statePosState,
                            state.stateParseErrors
                        )
                    )

                else ->
                    consumedOk(
                        res,
                        State(
                            tail,
                            state.stateContext,
                            state.stateOffset + 1,
                            state.statePosState,
                            state.stateParseErrors
                        ),
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
    override fun <B> unparser(
        state: State<Error, Context>,
        trampoline: (() -> B) -> B,
        consumedOk: ConsumedOk<Error, Context, String, B>,
        consumedError: ConsumedError<Error, Context, B>,
        emptyOk: EmptyOk<Error, Context, String, B>,
        emptyError: EmptyError<Error, Context, B>
    ): B {
        val unexpected: (Int, ErrorItem<Char>) -> ParseError<Error> = { pos, u ->
            ParseError.TrivialError(
                pos, u, setOf(ErrorItem.Tokens(tokens.toNonEmptyList()))
            )
        }

        return when (val taken = state.stateInput.takeN()) {
            null ->
                emptyError(
                    unexpected(state.stateOffset, ErrorItem.EndOfInput),
                    state
                )

            else -> {
                val (tts, tail) = taken

                if (test(tokens, tts)) {
                    val nextState =
                        State(
                            tail,
                            state.stateContext,
                            state.stateOffset + tokens.length,
                            state.statePosState,
                            state.stateParseErrors
                        )

                    if (tokens.isEmpty()) {
                        emptyOk(tts, nextState, Hints.empty())
                    } else {
                        consumedOk(tts, nextState, Hints.empty())
                    }
                } else {
                    emptyError(
                        unexpected(state.stateOffset, ErrorItem.Tokens(tts.toNonEmptyList())),
                        State(
                            state.stateInput,
                            state.stateContext,
                            state.stateOffset,
                            state.statePosState,
                            state.stateParseErrors
                        )
                    )
                }
            }
        }
    }
}


fun <Context> getContext(): ParsekT<Nothing, Context, Context> =
    object : ParsekT<Nothing, Context, Context> {
        override fun <B> unparser(
            state: State<Nothing, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Context, State<Nothing, Context>, Hints<Char>) -> B,
            consumedError: (ParseError<Nothing>, State<Nothing, Context>) -> B,
            emptyOk: (Context, State<Nothing, Context>, Hints<Char>) -> B,
            emptyError: (ParseError<Nothing>, State<Nothing, Context>) -> B
        ): B = trampoline { emptyOk(state.stateContext.context, state, Hints.empty()) }

    }


fun <Context> updateContext(fn: (Context) -> Context): ParserC<Context, Unit> =
    object : ParsekT<Nothing, Context, Unit> {
        override fun <B> unparser(
            state: State<Nothing, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Unit, State<Nothing, Context>, Hints<Char>) -> B,
            consumedError: (ParseError<Nothing>, State<Nothing, Context>) -> B,
            emptyOk: (Unit, State<Nothing, Context>, Hints<Char>) -> B,
            emptyError: (ParseError<Nothing>, State<Nothing, Context>) -> B
        ): B = trampoline {
            emptyOk(
                Unit,
                state.copy(stateContext = state.stateContext.copy(fn(state.stateContext.context))),
                Hints.empty()
            )
        }

    }


fun <Output> pure(pure: Output): Parser<Output> =
    object : Parser<Output> {
        override fun <B> unparser(
            state: State<Nothing, Nothing>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<Nothing, Nothing>, Hints<Char>) -> B,
            consumedError: (ParseError<Nothing>, State<Nothing, Nothing>) -> B,
            emptyOk: (Output, State<Nothing, Nothing>, Hints<Char>) -> B,
            emptyError: (ParseError<Nothing>, State<Nothing, Nothing>) -> B
        ): B = trampoline { emptyOk(pure, state, Hints.empty()) }
    }


fun <Error, Context, Output1, Output2> ParsekT<Error, Context, Output1>.bind(
    cont: (Output1) -> ParsekT<Error, Context, Output2>
): ParsekT<Error, Context, Output2> = object : ParsekT<Error, Context, Output2> {
    override fun <B> unparser(
        state: State<Error, Context>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output2, State<Error, Context>, Hints<Char>) -> B,
        consumedError: (ParseError<Error>, State<Error, Context>) -> B,
        emptyOk: (Output2, State<Error, Context>, Hints<Char>) -> B,
        emptyError: (ParseError<Error>, State<Error, Context>) -> B
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


internal typealias InitRec<Error, Context, Output> =
            () -> Trampoline<Error, Context, Output>

internal typealias StepRec<Error, Context, Output> =
        Step<InitRec<Error, Context, Output>, Reply<Error, Context, Output>>

internal typealias RunRec<Error, Context, Output> =
            (InitRec<Error, Context, Output>) -> StepRec<Error, Context, Output>


internal fun <Error, Context, Output> tailRec(
    next: RunRec<Error, Context, Output>,
    initial: InitRec<Error, Context, Output>
): Reply<Error, Context, Output> {
    tailrec fun loop(arg: () -> Trampoline<Error, Context, Output>): Reply<Error, Context, Output> =
        when (val k = next(arg)) {
            is Step.Done -> k.result
            is Step.Loop -> loop(k.loop)
        }

    return loop(initial)
}


fun <Error, Output> ParsekT<Error, Nothing, Output>.runParsekT(
    input: String,
    name: FilePath = FilePath.empty()
): Reply<Error, Unit, Output> = runParsekT(name = name, input = input, context = Unit)


fun <Error, Context, Output> ParsekT<Error, Context, Output>.runParsekT(
    input: String,
    name: FilePath = FilePath.empty(),
    context: Context,
    initialState: State<Error, Context> = State.initial(name, input, context)
): Reply<Error, Context, Output> {
    tailrec fun go(
        n: () -> Trampoline<Error, Context, Output>
    ): Step<() -> Trampoline<Error, Context, Output>, Reply<Error, Context, Output>> =
        when (val step = n()) {
            is Trampoline.Done -> Step.Done(step.done)
            is Trampoline.More -> go(step.run)
        }

    return tailRec(::go) {
        unparser(
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
