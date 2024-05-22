import Trampoline.Companion.done
import Trampoline.Companion.more
import arrow.core.NonEmptyList
import arrow.core.nonEmptyListOf


interface ParsekT<S : Stream<*, *>, out Error, in Context, out Output> {
    fun <B> unparser(
        state: State<S, @UnsafeVariance Error, Context>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output, State<S, @UnsafeVariance Error, @UnsafeVariance Context>, Hints<*>) -> B,
        consumedError: (ParseError<*, @UnsafeVariance Error>, State<S, @UnsafeVariance Error, @UnsafeVariance Context>) -> B,
        emptyOk: (Output, State<S, @UnsafeVariance Error, @UnsafeVariance Context>, Hints<*>) -> B,
        emptyError: (ParseError<*, @UnsafeVariance Error>, State<S, @UnsafeVariance Error, @UnsafeVariance Context>) -> B
    ): B
}

typealias ParserE<S, Error, Output> = ParsekT<S, Error, Any, Output>
typealias ParserC<S, Context, Output> = ParsekT<S, Nothing, Context, Output>
typealias Parser<S, Output> = ParsekT<S, Nothing, Any, Output>


fun <S : Stream<*, *>, Error, Context, Output> fail(message: String): ParsekT<S, Error, Context, Output> =
    object : ParsekT<S, Error, Context, Output> {
        override fun <B> unparser(
            state: State<S, Error, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Error, Context>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Error, Context>) -> B,
            emptyOk: (Output, State<S, Error, Context>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Error, Context>) -> B
        ): B = emptyError(
            ParseError.FancyError(state.stateOffset, setOf(ErrorFancy.ErrorFail(message))),
            state
        )


    }

fun <S : Stream<*, *>, Error, Context, Output> ParsekT<S, Error, Context, Output>.attempt() =
    object : ParsekT<S, Error, Context, Output> {
        override fun <B> unparser(
            state: State<S, Error, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Error, Context>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Error, Context>) -> B,
            emptyOk: (Output, State<S, Error, Context>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Error, Context>) -> B
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


fun <S : Stream<*, *>, Error, Context, Output> ParsekT<S, Error, Context, Output>.lookAhead() =
    object : ParsekT<S, Error, Context, Output> {
        override fun <B> unparser(
            state: State<S, Error, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Error, Context>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Error, Context>) -> B,
            emptyOk: (Output, State<S, Error, Context>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Error, Context>) -> B
        ): B = trampoline {
            this@lookAhead.unparser(
                state,
                trampoline,
                { a, _, _ -> emptyOk(a, state, Hints.empty()) },
                consumedError,
                { a, _, _ -> emptyOk(a, state, Hints.empty()) },
                emptyError
            )
        }
    }

fun <Token, S : Stream<*, Token>, Error, Context, Output> ParsekT<S, Error, Context, Output>.notFollowedBy() =
    object : ParsekT<S, Error, Context, Unit> {
        override fun <B> unparser(
            state: State<S, Error, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Unit, State<S, Error, Context>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Error, Context>) -> B,
            emptyOk: (Unit, State<S, Error, Context>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Error, Context>) -> B
        ): B {
            val what = state.stateInput.uncons()
                ?.first
                ?.let { ErrorItem.Tokens(nonEmptyListOf(it)) } ?: ErrorItem.EndOfInput

            fun unexpected(item: ErrorItem<Token>) =
                ParseError.TrivialError<S, Token>(state.stateOffset, item, emptySet())

            return trampoline {
                this@notFollowedBy.unparser(
                    state,
                    trampoline,
                    { _, _, _ -> emptyError(unexpected(what), state) },
                    { _, _ -> emptyOk(Unit, state, Hints.empty()) },
                    { _, _, _ -> emptyError(unexpected(what), state) },
                    { _, _ -> emptyOk(Unit, state, Hints.empty()) },
                )
            }
        }
    }


fun <S : Stream<*, *>, Error, Context, B> withHints(
    hints: Hints<*>,
    continuation: (ParseError<*, Error>, State<S, Error, Context>) -> B
): (ParseError<*, Error>, State<S, Error, Context>) -> B = { error, state ->
    when (error) {
        is ParseError.TrivialError<*, *> -> continuation(
            ParseError.TrivialError<S, Any?>(error.offset, error.unexpected, error.expected + hints.hints),
            state
        )

        else -> continuation(error, state)
    }
}

fun <S : Stream<*, *>, Error, Context, A, B> accHints(
    hints1: Hints<*>,
    okContinuation: (A, State<S, Error, Context>, Hints<*>) -> B,
): (A, State<S, Error, Context>, Hints<*>) -> B = { input, state, hints2 ->
    okContinuation(input, state, Hints(hints1.hints + hints2.hints))
}


fun <Token> refreshHints(
    hints: Hints<Token>,
    error: ErrorItem<Token>?
) = when {
    error == null -> Hints.empty()
    hints.hints.isEmpty() -> hints
    else -> Hints(setOf(error))
}


fun <Tokens : Any, Token : Any, Error, Context, Output> token(
    test: (Token) -> Output?,
    errorItems: Set<ErrorItem<Token>>
): ParsekT<Stream<Tokens, Token>, Error, Context, Output> =
    object : ParsekT<Stream<Tokens, Token>, Error, Context, Output> {
        override fun <B> unparser(
            state: State<Stream<Tokens, Token>, Error, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<Stream<Tokens, Token>, Error, Context>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<Stream<Tokens, Token>, Error, Context>) -> B,
            emptyOk: (Output, State<Stream<Tokens, Token>, Error, Context>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<Stream<Tokens, Token>, Error, Context>) -> B
        ): B = when (val taken = state.stateInput.uncons()) {
            null ->
                emptyError(
                    ParseError.TrivialError<Stream<Tokens, Token>, Token>(
                        state.stateOffset,
                        ErrorItem.EndOfInput,
                        errorItems
                    ),
                    state
                )

            else -> {
                val (current, tail) = taken

                when (val res = test(current)) {
                    null ->
                        emptyError(
                            ParseError.TrivialError<Stream<Tokens, Token>, Token>(
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


internal fun <Tokens : Any, Token : Any, Error, Context> tokens(
    test: (Tokens, Tokens) -> Boolean,
    length: (Tokens) -> Int,
    toNonEmptyList: (Tokens) -> NonEmptyList<Token>,
    isEmpty: (Tokens) -> Boolean,
    tokens: Tokens
) =
    object : ParsekT<Stream<Tokens, Token>, Error, Context, Tokens> {
        override fun <B> unparser(
            state: State<Stream<Tokens, Token>, Error, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Tokens, State<Stream<Tokens, Token>, Error, Context>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<Stream<Tokens, Token>, Error, Context>) -> B,
            emptyOk: (Tokens, State<Stream<Tokens, Token>, Error, Context>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<Stream<Tokens, Token>, Error, Context>) -> B
        ): B {
            val unexpected: (Int, ErrorItem<Token>) -> ParseError<Token, Error> = { pos, u ->
                ParseError.TrivialError(
                    pos, u, setOf(ErrorItem.Tokens(toNonEmptyList(tokens)))
                )
            }

            val tokenLength = length(tokens)
            return when (val taken = state.stateInput.takeN(tokenLength)) {
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
                                state.stateOffset + tokenLength,
                                state.statePosState,
                                state.stateParseErrors
                            )

                        if (isEmpty(tokens)) {
                            emptyOk(tts, nextState, Hints.empty())
                        } else {
                            consumedOk(tts, nextState, Hints.empty())
                        }

                    } else {
                        emptyError(
                            unexpected(state.stateOffset, ErrorItem.Tokens(toNonEmptyList(tts))),
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

fun <S : Any> satisfy(test: (Char) -> Boolean): Parser<Stream<S, Char>, Char> =
    token(
        test = { if (test(it)) it else null },
        errorItems = setOf(ErrorItem.Tokens(nonEmptyListOf('a')))
    )

fun <S : Any> char(char: Char): Parser<Stream<S, Char>, Char> =
    token(
        test = { if (it == char) it else null },
        errorItems = setOf(ErrorItem.Tokens(nonEmptyListOf(char)))
    )


fun <S : Stream<*, *>, Context> getContext(): ParserC<S, Context, Context> =
    object : ParserC<S, Context, Context> {
        override fun <B> unparser(
            state: State<S, Nothing, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Context, State<S, Nothing, Context>, Hints<*>) -> B,
            consumedError: (ParseError<*, Nothing>, State<S, Nothing, Context>) -> B,
            emptyOk: (Context, State<S, Nothing, Context>, Hints<*>) -> B,
            emptyError: (ParseError<*, Nothing>, State<S, Nothing, Context>) -> B
        ): B = trampoline { emptyOk(state.stateContext.context, state, Hints.empty()) }
    }


fun <S : Stream<*, *>, Error, Context> updateContext(fn: (Context) -> Context): ParsekT<S, Error, Context, Unit> =
    object : ParsekT<S, Error, Context, Unit> {
        override fun <B> unparser(
            state: State<S, Error, Context>,
            trampoline: (() -> B) -> B,
            consumedOk: (Unit, State<S, Error, Context>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Error, Context>) -> B,
            emptyOk: (Unit, State<S, Error, Context>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Error, Context>) -> B
        ): B = trampoline {
            emptyOk(
                Unit,
                state.copy(stateContext = state.stateContext.copy(fn(state.stateContext.context))),
                Hints.empty()
            )
        }
    }


fun <S : Stream<*, *>, Output> pure(pure: Output): Parser<S, Output> =
    object : Parser<S, Output> {
        override fun <B> unparser(
            state: State<S, Nothing, Any>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Nothing, Any>, Hints<*>) -> B,
            consumedError: (ParseError<*, Nothing>, State<S, Nothing, Any>) -> B,
            emptyOk: (Output, State<S, Nothing, Any>, Hints<*>) -> B,
            emptyError: (ParseError<*, Nothing>, State<S, Nothing, Any>) -> B
        ): B = trampoline { emptyOk(pure, state, Hints.empty()) }

    }

fun string(str: String): Parser<StringStream, String> = pure(str)


infix fun <S : Stream<*, *>, Error, Context, Output1, Output2> ParsekT<S, Error, Context, Output1>.map(
    fn: (Output1) -> Output2
) = object : ParsekT<S, Error, Context, Output2> {
    override fun <B> unparser(
        state: State<S, Error, Context>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output2, State<S, Error, Context>, Hints<*>) -> B,
        consumedError: (ParseError<*, Error>, State<S, Error, Context>) -> B,
        emptyOk: (Output2, State<S, Error, Context>, Hints<*>) -> B,
        emptyError: (ParseError<*, Error>, State<S, Error, Context>) -> B
    ): B = trampoline {
        this@map.unparser(
            state,
            trampoline,
            { a, b, c ->
                trampoline {
                    consumedOk(fn(a), b, c)
                }
            },
            consumedError,
            { a, b, c ->
                trampoline {
                    emptyOk(fn(a), b, c)
                }
            },
            emptyError
        )
    }
}

infix fun <S : Stream<*, *>, Error, Context, Output1> ParsekT<S, Error, Context, Output1>.or(
    or: ParsekT<S, Error, Context, Output1>
): ParsekT<S, Error, Context, Output1> = alt(or)

internal fun <S : Stream<*, *>, Error, Context, Output1> ParsekT<S, Error, Context, Output1>.alt(
    or: ParsekT<S, Error, Context, Output1>
): ParsekT<S, Error, Context, Output1> = object : ParsekT<S, Error, Context, Output1> {
    override fun <B> unparser(
        state: State<S, Error, Context>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output1, State<S, Error, Context>, Hints<*>) -> B,
        consumedError: (ParseError<*, Error>, State<S, Error, Context>) -> B,
        emptyOk: (Output1, State<S, Error, Context>, Hints<*>) -> B,
        emptyError: (ParseError<*, Error>, State<S, Error, Context>) -> B
    ): B = trampoline {
        this@alt.unparser(
            state,
            trampoline,
            consumedOk,
            consumedError,
            emptyOk
        ) { err, ms ->
            or.unparser(
                state,
                trampoline,
                consumedOk,
                { a, b ->
                    trampoline { consumedError(a + err, ms longestMatch b) }
                },
                { a, b, c ->
                    trampoline { emptyOk(a, b, c) } // TODO : Merge hints
                },
                { a, b ->
                    trampoline { emptyError(a + err, ms longestMatch b) }
                }
            )
        }
    }

}


fun <S : Stream<*, *>, Error, Context, Output1, Output2> ParsekT<S, Error, Context, Output1>.flatMap(
    fn: (Output1) -> ParsekT<S, Error, Context, Output2>
): ParsekT<S, Error, Context, Output2> = bind(fn)


internal fun <S : Stream<*, *>, Error, Context, Output1, Output2> ParsekT<S, Error, Context, Output1>.bind(
    cont: (Output1) -> ParsekT<S, Error, Context, Output2>
): ParsekT<S, Error, Context, Output2> = object : ParsekT<S, Error, Context, Output2> {
    override fun <B> unparser(
        state: State<S, Error, Context>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output2, State<S, Error, Context>, Hints<*>) -> B,
        consumedError: (ParseError<*, Error>, State<S, Error, Context>) -> B,
        emptyOk: (Output2, State<S, Error, Context>, Hints<*>) -> B,
        emptyError: (ParseError<*, Error>, State<S, Error, Context>) -> B
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
                        accHints(c, consumedOk),
                        withHints(c, consumedError)
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
                        accHints(c, emptyOk),
                        withHints(c, emptyError)
                    )
                }
            },
            emptyError
        )
    }
}

internal fun <S : Stream<*, *>, Output> zero(): Parser<S, Output> =
    object : Parser<S, Output> {
        override fun <B> unparser(
            state: State<S, Nothing, Any>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Nothing, Any>, Hints<*>) -> B,
            consumedError: (ParseError<*, Nothing>, State<S, Nothing, Any>) -> B,
            emptyOk: (Output, State<S, Nothing, Any>, Hints<*>) -> B,
            emptyError: (ParseError<*, Nothing>, State<S, Nothing, Any>) -> B
        ): B = emptyError(ParseError.TrivialError<S, Any>(state.stateOffset, null, emptySet()), state)
    }


internal typealias InitRec<Stream, Error, Context, Output> =
            () -> Trampoline<Stream, Error, Context, Output>

internal typealias StepRec<Stream, Error, Context, Output> =
        Step<InitRec<Stream, Error, Context, Output>, Reply<Stream, Error, Context, Output>>

internal typealias RunRec<Stream, Error, Context, Output> =
            (InitRec<Stream, Error, Context, Output>) -> StepRec<Stream, Error, Context, Output>


internal fun <S : Stream<*, *>, Error, Context, Output> tailRec(
    next: RunRec<S, Error, Context, Output>,
    initial: InitRec<S, Error, Context, Output>
): Reply<S, Error, Context, Output> {
    tailrec fun loop(arg: () -> Trampoline<S, Error, Context, Output>): Reply<S, Error, Context, Output> =
        when (val k = next(arg)) {
            is Step.Done -> k.result
            is Step.Loop -> loop(k.loop)
        }

    return loop(initial)
}


fun <S : Stream<*, *>, Error, Output> ParsekT<S, Error, Any, Output>.runParsekT(
    input: S,
    name: FilePath = FilePath.empty()
): Reply<S, Error, Any, Output> =
    runParsekT(
        name = name,
        input = input,
        context = Unit,
        initialState = State.initial(name, input, Unit)
    )

fun <S : Stream<*, *>, Error, Context, Output> ParsekT<S, Error, Context, Output>.runParsekT(
    input: S,
    name: FilePath = FilePath.empty(),
    context: Context,
    initialState: State<S, Error, Context> = State.initial(name, input, context)
): Reply<S, Error, Context, Output> {
    tailrec fun go(
        n: () -> Trampoline<S, Error, Context, Output>
    ): Step<() -> Trampoline<S, Error, Context, Output>, Reply<S, Error, Context, Output>> =
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
