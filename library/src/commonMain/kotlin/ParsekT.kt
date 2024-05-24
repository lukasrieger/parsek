import Trampoline.Companion.done
import Trampoline.Companion.more
import arrow.core.*
import error.*
import stream.Stream
import kotlin.math.max


interface ParsekT<in S : Stream<*, *>, in Context, out Error, out Output> {
    operator fun <B> invoke(
        state: State<@UnsafeVariance S, Context, @UnsafeVariance Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output, State<@UnsafeVariance S, @UnsafeVariance Context, @UnsafeVariance Error>, Hints<*>) -> B,
        consumedError: (ParseError<*, @UnsafeVariance Error>, State<@UnsafeVariance S, @UnsafeVariance Context, @UnsafeVariance Error>) -> B,
        emptyOk: (Output, State<@UnsafeVariance S, @UnsafeVariance Context, @UnsafeVariance Error>, Hints<*>) -> B,
        emptyError: (ParseError<*, @UnsafeVariance Error>, State<@UnsafeVariance S, @UnsafeVariance Context, @UnsafeVariance Error>) -> B
    ): B
}

typealias ParserE<S, Error, Output> = ParsekT<S, Any, Error, Output>
typealias ParserC<S, Context, Output> = ParsekT<S, Context, Nothing, Output>
typealias Parser<S, Output> = ParsekT<S, Any, Nothing, Output>


fun <S : Stream<*, *>, Context, Error, Output> fail(message: String): ParsekT<S, Context, Error, Output> =
    object : ParsekT<S, Context, Error, Output> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
            emptyOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
        ): B = emptyError(
            ParseError.FancyError(state.stateOffset, setOf(ErrorFancy.ErrorFail(message))),
            state
        )

    }

fun <S : Stream<*, *>, Context, Error, Output> ParsekT<S, Context, Error, Output>.attempt() =
    object : ParsekT<S, Context, Error, Output> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
            emptyOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
        ): B = trampoline {
            this@attempt(
                state,
                trampoline,
                consumedOk,
                { err, _ -> emptyError(err, state) },
                emptyOk,
                { err, _ -> emptyError(err, state) }
            )
        }

    }


fun <S : Stream<*, *>, Context, Error, Output> ParsekT<S, Context, Error, Output>.lookAhead() =
    object : ParsekT<S, Context, Error, Output> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
            emptyOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
        ): B = trampoline {
            this@lookAhead(
                state,
                trampoline,
                { a, _, _ -> emptyOk(a, state, Hints.empty()) },
                consumedError,
                { a, _, _ -> emptyOk(a, state, Hints.empty()) },
                emptyError
            )
        }

    }

fun <Token, S : Stream<*, Token>, Context, Error, Output> ParsekT<S, Context, Error, Output>.notFollowedBy() =
    object : ParsekT<S, Context, Error, Unit> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Unit, State<S, Context, Error>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
            emptyOk: (Unit, State<S, Context, Error>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
        ): B {
            val what = state.stateInput.uncons()
                ?.first
                ?.let { ErrorItem.Tokens(nonEmptyListOf(it)) } ?: ErrorItem.EndOfInput

            fun unexpected(item: ErrorItem<Token>) =
                ParseError.TrivialError<S, Token>(state.stateOffset, item, emptySet())

            return trampoline {
                this@notFollowedBy(
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


fun <S : Stream<*, *>, Context, Error, B> withHints(
    hints: Hints<*>,
    continuation: (ParseError<*, Error>, State<S, Context, Error>) -> B
): (ParseError<*, Error>, State<S, Context, Error>) -> B = { error, state ->
    when (error) {
        is ParseError.TrivialError<*, *> -> continuation(
            ParseError.TrivialError<S, Any?>(error.offset, error.unexpected, error.expected + hints.hints),
            state
        )

        else -> continuation(error, state)
    }
}

fun <S : Stream<*, *>, Context, Error, A, B> accHints(
    hints1: Hints<*>,
    okContinuation: (A, State<S, Context, Error>, Hints<*>) -> B,
): (A, State<S, Context, Error>, Hints<*>) -> B = { input, state, hints2 ->
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


fun <Tokens : Any, Token : Any, Context, Error, Output> token(
    test: (Token) -> Output?,
    errorItems: Set<ErrorItem<Token>>
): ParsekT<Stream<Tokens, Token>, Context, Error, Output> =
    object : ParsekT<Stream<Tokens, Token>, Context, Error, Output> {
        override fun <B> invoke(
            state: State<Stream<Tokens, Token>, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<Stream<Tokens, Token>, Context, Error>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<Stream<Tokens, Token>, Context, Error>) -> B,
            emptyOk: (Output, State<Stream<Tokens, Token>, Context, Error>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<Stream<Tokens, Token>, Context, Error>) -> B
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


internal fun <Tokens : Any, Token : Any, Context, Error> tokens(
    test: (Tokens, Tokens) -> Boolean,
    length: (Tokens) -> Int,
    toNonEmptyList: (Tokens) -> NonEmptyList<Token>,
    isEmpty: (Tokens) -> Boolean,
    tokens: Tokens
) =
    object : ParsekT<Stream<Tokens, Token>, Context, Error, Tokens> {
        override fun <B> invoke(
            state: State<Stream<Tokens, Token>, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Tokens, State<Stream<Tokens, Token>, Context, Error>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<Stream<Tokens, Token>, Context, Error>) -> B,
            emptyOk: (Tokens, State<Stream<Tokens, Token>, Context, Error>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<Stream<Tokens, Token>, Context, Error>) -> B
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

fun <S : Stream<*, *>> eof(): Parser<S, Unit> =
    object : Parser<S, Unit> {
        override fun <B> invoke(
            state: State<S, Any, Nothing>,
            trampoline: (() -> B) -> B,
            consumedOk: (Unit, State<S, Any, Nothing>, Hints<*>) -> B,
            consumedError: (ParseError<*, Nothing>, State<S, Any, Nothing>) -> B,
            emptyOk: (Unit, State<S, Any, Nothing>, Hints<*>) -> B,
            emptyError: (ParseError<*, Nothing>, State<S, Any, Nothing>) -> B
        ): B = trampoline {
            when (val x = state.stateInput.uncons()) {
                null -> emptyOk(Unit, state, Hints.empty())
                else -> {
                    val us = ErrorItem.Tokens(nonEmptyListOf(x.first))
                    val ps = setOf(ErrorItem.EndOfInput)

                    emptyError(
                        ParseError.TrivialError<S, _>(state.stateOffset, us, ps),
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


fun <Token : Any, Tokens : Any, S : Stream<Tokens, Token>> pTakeWhile(
    ml: String?,
    test: (Token) -> Boolean
): Parser<Stream<Tokens, Token>, Tokens> = object : Parser<Stream<Tokens, Token>, Tokens> {
    override fun <B> invoke(
        state: State<Stream<Tokens, Token>, Any, Nothing>,
        trampoline: (() -> B) -> B,
        consumedOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints<*>) -> B,
        consumedError: (ParseError<*, Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B,
        emptyOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints<*>) -> B,
        emptyError: (ParseError<*, Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B
    ): B = trampoline {
        val (ts, nextInput) = state.stateInput.takeWhile(test)
        val len = state.stateInput.chunkLength(ts)
        val hs = when (ml) {
            null -> Hints.empty()
            else -> Hints(setOf(ErrorItem.Label(ml)))
        }

        when (state.stateInput.chunkEmpty(ts)) {
            true -> emptyOk(
                ts,
                State(
                    nextInput,
                    state.stateContext,
                    state.stateOffset + len,
                    state.statePosState,
                    state.stateParseErrors
                ),
                hs
            )

            false -> consumedOk(
                ts,
                State(
                    nextInput,
                    state.stateContext,
                    state.stateOffset + len,
                    state.statePosState,
                    state.stateParseErrors
                ),
                hs
            )
        }
    }

}


fun <Token : Any, Tokens : Any, S : Stream<Tokens, Token>> pTakeWhile1(
    ml: String?,
    test: (Token) -> Boolean
): Parser<Stream<Tokens, Token>, Tokens> = object : Parser<Stream<Tokens, Token>, Tokens> {
    override fun <B> invoke(
        state: State<Stream<Tokens, Token>, Any, Nothing>,
        trampoline: (() -> B) -> B,
        consumedOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints<*>) -> B,
        consumedError: (ParseError<*, Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B,
        emptyOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints<*>) -> B,
        emptyError: (ParseError<*, Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B
    ): B = trampoline {
        val (ts, nextInput) = state.stateInput.takeWhile(test)
        val len = state.stateInput.chunkLength(ts)
        val el = ml?.let { ErrorItem.Label(it) }

        val hs = when (el) {
            null -> Hints.empty()
            else -> Hints(setOf(el))
        }

        when (state.stateInput.chunkEmpty(ts)) {
            true -> {
                val us = when (val t = state.stateInput.uncons()) {
                    null -> ErrorItem.EndOfInput
                    else -> ErrorItem.Tokens(nonEmptyListOf(t.first))
                }

                val ps = el?.let(::setOf) ?: emptySet()

                emptyError(
                    ParseError.TrivialError<S, _>(state.stateOffset, us, ps),
                    State(
                        state.stateInput,
                        state.stateContext,
                        state.stateOffset,
                        state.statePosState,
                        state.stateParseErrors
                    )
                )
            }

            else -> consumedOk(
                ts,
                State(
                    nextInput,
                    state.stateContext,
                    state.stateOffset + len,
                    state.statePosState,
                    state.stateParseErrors
                ),
                hs
            )
        }
    }
}

fun <Token : Any, Tokens : Any, S : Stream<Tokens, Token>> pTake(
    ml: String?,
    count: Int
): Parser<Stream<Tokens, Token>, Tokens> = object : Parser<Stream<Tokens, Token>, Tokens> {
    override fun <B> invoke(
        state: State<Stream<Tokens, Token>, Any, Nothing>,
        trampoline: (() -> B) -> B,
        consumedOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints<*>) -> B,
        consumedError: (ParseError<*, Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B,
        emptyOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints<*>) -> B,
        emptyError: (ParseError<*, Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B
    ): B = trampoline {
        val n = max(0, count)
        val el = ml?.let { ErrorItem.Label(it) }
        val ps = el?.let(::setOf) ?: emptySet()

        when (val taken = state.stateInput.takeN(n)) {
            null -> emptyError(
                ParseError.TrivialError<S, _>(state.stateOffset, ErrorItem.EndOfInput, ps), state
            )

            else -> {
                val (ts, nextInput) = taken
                val len = state.stateInput.chunkLength(ts)
                if (len != count) {
                    emptyError(
                        ParseError.TrivialError<S, _>(state.stateOffset + len, ErrorItem.EndOfInput, ps),
                        State(
                            state.stateInput,
                            state.stateContext,
                            state.stateOffset,
                            state.statePosState,
                            state.stateParseErrors
                        )
                    )
                } else {
                    consumedOk(
                        ts,
                        State(
                            nextInput,
                            state.stateContext,
                            state.stateOffset + len,
                            state.statePosState,
                            state.stateParseErrors
                        ),
                        Hints.empty()
                    )
                }
            }
        }
    }
}


fun <S : Stream<*, *>, Error> parseError(error: ParseError<S, Error>) =
    object : ParserE<S, Error, Nothing> {
        override fun <B> invoke(
            state: State<S, Any, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Nothing, State<S, Any, Error>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Any, Error>) -> B,
            emptyOk: (Nothing, State<S, Any, Error>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Any, Error>) -> B
        ): B = trampoline { emptyError(error, state) }
    }


infix fun <S : Stream<*, *>, Context, Error, Output> ParsekT<S, Context, Error, Output>.label(
    label: String
) = object : ParsekT<S, Context, Error, Output> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
        consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
        emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
    ): B = trampoline {
        val el = label.takeIf { it.isNotEmpty() }?.let { ErrorItem.Label(it) }

        val consumedOkL: (Output, State<S, Context, Error>, Hints<*>) -> B = { a, b, c ->
            when (el) {
                null -> consumedOk(a, b, refreshHints(c, null))
                else -> consumedOk(a, b, c)
            }
        }

        val emptyOkL: (Output, State<S, Context, Error>, Hints<*>) -> B = { a, b, c ->
            emptyOk(a, b, refreshHints(c, el))
        }

        val emptyErrorL: (ParseError<*, Error>, State<S, Context, Error>) -> B = { a, b ->
            emptyError(
                when (a) {
                    is ParseError.TrivialError<*, *> ->
                        ParseError.TrivialError<S, _>(
                            a.offset,
                            a.unexpected,
                            setOfNotNull(el)
                        )

                    else -> a
                },
                b
            )
        }

        this@label(
            state,
            trampoline,
            consumedOkL,
            consumedError,
            emptyOkL,
            emptyErrorL
        )
    }
}


fun <S : Stream<*, *>, Context, Error, Output> ParsekT<S, Context, Error, Output>.withRecovery(
    recoverBy: (ParseError<*, Error>) -> ParsekT<S, Context, Error, Output>
) = object : ParsekT<S, Context, Error, Output> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
        consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output, State<S, Context, Error>, Hints<*>) -> B,
        emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
    ): B = trampoline {
        fun consumedErrorM(error: ParseError<*, Error>, s: State<S, Context, Error>) =
            trampoline {
                recoverBy(error)(
                    s,
                    trampoline,
                    { a, b, _ -> consumedOk(a, b, Hints.empty()) },
                    { _, _ -> consumedError(error, s) },
                    { a, b, _ -> emptyOk(a, b, Hints.toHints(b.stateOffset, error)) },
                    { _, _ -> consumedError(error, s) }
                )
            }


        fun emptyErrorM(error: ParseError<*, Error>, s: State<S, Context, Error>) =
            trampoline {
                recoverBy(error)(
                    s,
                    trampoline,
                    { a, b, _ -> consumedOk(a, b, Hints.toHints(b.stateOffset, error)) },
                    { _, _ -> consumedError(error, s) },
                    { a, b, _ -> emptyOk(a, b, Hints.toHints(b.stateOffset, error)) },
                    { _, _ -> consumedError(error, s) }
                )
            }


        this@withRecovery(
            state,
            trampoline,
            consumedOk,
            ::consumedErrorM,
            emptyOk,
            ::emptyErrorM
        )
    }
}

fun <S : Stream<*, *>, Error, Context, Output> ParsekT<S, Context, Error, Output>.observing(): ParsekT<S, Context, Error, Either<ParseError<S, Error>, Output>> =
    object : ParsekT<S, Context, Error, Either<ParseError<S, Error>, Output>> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Either<ParseError<S, Error>, Output>, State<S, Context, Error>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
            emptyOk: (Either<ParseError<S, Error>, Output>, State<S, Context, Error>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
        ): B = trampoline {
            fun consumedErrC(error: ParseError<S, Error>, s: State<S, Context, Error>) =
                consumedOk(Either.Left(error), s, Hints.empty())

            fun emptyErrorC(error: ParseError<S, Error>, s: State<S, Context, Error>) =
                emptyOk(Either.Left(error), s, Hints.toHints(s.stateOffset, error))

            @Suppress("unchecked_cast")
            this@observing(
                state,
                trampoline,
                { a, b, c -> consumedOk(Either.Right(a), b, c) },
                ::consumedErrC as (ParseError<*, Error>, State<S, Context, Error>) -> B,
                { a, b, c -> emptyOk(Either.Right(a), b, c) },
                ::emptyErrorC as (ParseError<*, Error>, State<S, Context, Error>) -> B
            )
        }
    }


fun <S : Any> satisfy(test: (Char) -> Boolean): Parser<Stream<S, Char>, Char> =
    token(
        test = { if (test(it)) it else null },
        errorItems = emptySet()
    )

fun <S : Any> char(char: Char): Parser<Stream<S, Char>, Char> =
    token(
        test = { if (it == char) it else null },
        errorItems = setOf(ErrorItem.Tokens(nonEmptyListOf(char)))
    )


fun <S : Stream<*, *>, Context> getContext(): ParserC<S, Context, Context> =
    object : ParserC<S, Context, Context> {
        override fun <B> invoke(
            state: State<S, Context, Nothing>,
            trampoline: (() -> B) -> B,
            consumedOk: (Context, State<S, Context, Nothing>, Hints<*>) -> B,
            consumedError: (ParseError<*, Nothing>, State<S, Context, Nothing>) -> B,
            emptyOk: (Context, State<S, Context, Nothing>, Hints<*>) -> B,
            emptyError: (ParseError<*, Nothing>, State<S, Context, Nothing>) -> B
        ): B = trampoline { emptyOk(state.stateContext.context, state, Hints.empty()) }
    }


fun <S : Stream<*, *>, Context, Error> updateContext(fn: (Context) -> Context): ParsekT<S, Context, Error, Unit> =
    object : ParsekT<S, Context, Error, Unit> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Unit, State<S, Context, Error>, Hints<*>) -> B,
            consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
            emptyOk: (Unit, State<S, Context, Error>, Hints<*>) -> B,
            emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
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
        override fun <B> invoke(
            state: State<S, Any, Nothing>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Any, Nothing>, Hints<*>) -> B,
            consumedError: (ParseError<*, Nothing>, State<S, Any, Nothing>) -> B,
            emptyOk: (Output, State<S, Any, Nothing>, Hints<*>) -> B,
            emptyError: (ParseError<*, Nothing>, State<S, Any, Nothing>) -> B
        ): B = trampoline { emptyOk(pure, state, Hints.empty()) }
    }


infix fun <S : Stream<*, *>, Context, Error, Output1, Output2> ParsekT<S, Context, Error, Output1>.map(
    fn: (Output1) -> Output2
) = object : ParsekT<S, Context, Error, Output2> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output2, State<S, Context, Error>, Hints<*>) -> B,
        consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output2, State<S, Context, Error>, Hints<*>) -> B,
        emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
    ): B = trampoline {
        this@map.invoke(
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

infix fun <S : Stream<*, *>, Context, Error, Output1> ParsekT<S, Context, Error, Output1>.or(
    or: ParsekT<S, Context, Error, Output1>
): ParsekT<S, Context, Error, Output1> = alt(or)

internal fun <S : Stream<*, *>, Context, Error, Output1> ParsekT<S, Context, Error, Output1>.alt(
    or: ParsekT<S, Context, Error, Output1>
): ParsekT<S, Context, Error, Output1> = object : ParsekT<S, Context, Error, Output1> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output1, State<S, Context, Error>, Hints<*>) -> B,
        consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output1, State<S, Context, Error>, Hints<*>) -> B,
        emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
    ): B = trampoline {
        this@alt(
            state,
            trampoline,
            consumedOk,
            consumedError,
            emptyOk
        ) { err, ms ->
            or(
                state,
                trampoline,
                consumedOk,
                { a, b ->
                    trampoline { consumedError(a + err, ms longestMatch b) }
                },
                { a, b, c ->
                    trampoline { emptyOk(a, b, Hints.toHints(b.stateOffset, err) + c) }
                },
                { a, b ->
                    trampoline { emptyError(a + err, ms longestMatch b) }
                }
            )
        }
    }
}


fun <S : Stream<*, *>, Context, Error, Output1, Output2> ParsekT<S, Context, Error, Output1>.flatMap(
    fn: (Output1) -> ParsekT<S, Context, Error, Output2>
): ParsekT<S, Context, Error, Output2> = bind(fn)


internal fun <S : Stream<*, *>, Context, Error, Output1, Output2> ParsekT<S, Context, Error, Output1>.bind(
    cont: (Output1) -> ParsekT<S, Context, Error, Output2>
): ParsekT<S, Context, Error, Output2> = object : ParsekT<S, Context, Error, Output2> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output2, State<S, Context, Error>, Hints<*>) -> B,
        consumedError: (ParseError<*, Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output2, State<S, Context, Error>, Hints<*>) -> B,
        emptyError: (ParseError<*, Error>, State<S, Context, Error>) -> B
    ): B = trampoline {
        this@bind(
            state,
            trampoline,
            { a, b, c ->
                trampoline {
                    cont(a)(
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
                    cont(a)(
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
        override fun <B> invoke(
            state: State<S, Any, Nothing>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Any, Nothing>, Hints<*>) -> B,
            consumedError: (ParseError<*, Nothing>, State<S, Any, Nothing>) -> B,
            emptyOk: (Output, State<S, Any, Nothing>, Hints<*>) -> B,
            emptyError: (ParseError<*, Nothing>, State<S, Any, Nothing>) -> B
        ): B = emptyError(ParseError.TrivialError<S, Any>(state.stateOffset, null, emptySet()), state)
    }


internal typealias InitRec<Stream, Context, Error, Output> =
            () -> Trampoline<Stream, Context, Error, Output>

internal typealias StepRec<Stream, Context, Error, Output> =
        Step<InitRec<Stream, Context, Error, Output>, Reply<Stream, Context, Error, Output>>

internal typealias RunRec<Stream, Context, Error, Output> =
            (InitRec<Stream, Context, Error, Output>) -> StepRec<Stream, Context, Error, Output>


internal fun <S : Stream<*, *>, Context, Error, Output> tailRec(
    next: RunRec<S, Context, Error, Output>,
    initial: InitRec<S, Context, Error, Output>
): Reply<S, Context, Error, Output> {
    tailrec fun loop(arg: () -> Trampoline<S, Context, Error, Output>): Reply<S, Context, Error, Output> =
        when (val k = next(arg)) {
            is Step.Done -> k.result
            is Step.Loop -> loop(k.loop)
        }

    return loop(initial)
}


fun <S : Stream<*, *>, Error, Output> ParsekT<S, Any, Error, Output>.runParsekT(
    input: S,
    name: FilePath = FilePath.empty()
): Reply<S, Any, Error, Output> =
    runParsekT(
        name = name,
        input = input,
        context = Unit,
        initialState = State.initial(name, input, Unit)
    )

fun <S : Stream<*, *>, Context, Error, Output> ParsekT<S, Context, Error, Output>.runParsekT(
    input: S,
    name: FilePath = FilePath.empty(),
    context: Context,
    initialState: State<S, Context, Error> = State.initial(name, input, context)
): Reply<S, Context, Error, Output> {
    tailrec fun go(
        n: () -> Trampoline<S, Context, Error, Output>
    ): Step<() -> Trampoline<S, Context, Error, Output>, Reply<S, Context, Error, Output>> =
        when (val step = n()) {
            is Trampoline.Done -> Step.Done(step.done)
            is Trampoline.More -> go(step.run)
        }

    return tailRec(::go) {
        this@runParsekT(
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

fun <S : Stream<*, *>, Error, Output> ParsekT<S, Any, Error, Output>.runParser(
    input: S,
    name: FilePath = FilePath.empty()
): Either<ParseErrorBundle<S, Error>, Output> {
    val reply = runParsekT(input, name)

    fun toBundle(errors: NonEmptyList<ParseError<S, Error>>) = ParseErrorBundle(
        bundleErrors = errors.sortedBy { it.offset }.toNonEmptyListOrNull()!!,
        bundlePosState = reply.state.statePosState
    )

    @Suppress("unchecked_cast")
    return when (reply.result) {
        is Result.Error<*, *> -> Either.Left(toBundle(nonEmptyListOf(reply.result.error) as NonEmptyList<ParseError<S, Error>>))
        is Result.Ok -> Either.Right(reply.result.result)
    }
}




