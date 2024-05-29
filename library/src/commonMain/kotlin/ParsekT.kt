import Trampoline.Companion.done
import Trampoline.Companion.more
import arrow.core.*
import error.*
import stream.Stream
import kotlin.jvm.JvmName
import kotlin.math.max
import kotlin.reflect.KProperty0


interface ParsekT<in S : Stream<*, *>, in Context, out Error, out Output> {
    operator fun <B> invoke(
        state: State<@UnsafeVariance S, Context, @UnsafeVariance Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output, State<S, @UnsafeVariance Context, Error>, Hints) -> B,
        consumedError: (ParseError<Error>, State<S, @UnsafeVariance Context, Error>) -> B,
        emptyOk: (Output, State<S, @UnsafeVariance Context, Error>, Hints) -> B,
        emptyError: (ParseError<Error>, State<S, @UnsafeVariance Context, Error>) -> B
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
            consumedOk: (Output, State<S, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
            emptyOk: (Output, State<S, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
        ): B = emptyError(
            ParseError.FancyError(state.stateOffset, setOf(ErrorFancy.ErrorFail(message))),
            state
        )

    }

inline fun <S : Stream<*, *>, Context, Error, Output> ParsekT<S, Context, Error, Output>.attempt() =
    object : ParsekT<S, Context, Error, Output> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
            emptyOk: (Output, State<S, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
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


inline fun <S : Stream<*, *>, Context, Error, Output> ParsekT<S, Context, Error, Output>.lookAhead() =
    object : ParsekT<S, Context, Error, Output> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
            emptyOk: (Output, State<S, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
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

inline fun <Token, S : Stream<*, Token>, Context, Error, Output> ParsekT<S, Context, Error, Output>.notFollowedBy() =
    object : ParsekT<S, Context, Error, Unit> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Unit, State<S, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
            emptyOk: (Unit, State<S, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
        ): B {
            val what = state.stateInput.uncons()
                ?.first
                ?.let { ErrorItem.Tokens(nonEmptyListOf(it)) } ?: ErrorItem.EndOfInput

            val unexpected: (item: ErrorItem<Token>) -> ParseError.TrivialError<Token> = { item ->
                ParseError.TrivialError(state.stateOffset, item, emptySet())
            }

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


inline fun <S : Stream<*, *>, Context, Error, B> withHints(
    hints: Hints,
    crossinline continuation: (ParseError<Error>, State<S, Context, Error>) -> B
): (ParseError<Error>, State<S, Context, Error>) -> B = { error, state ->
    when (error) {
        is ParseError.TrivialError<*> -> continuation(
            ParseError.TrivialError(error.offset, error.unexpected, error.expected + hints.hints),
            state
        )

        else -> continuation(error, state)
    }
}

inline fun <S : Stream<*, *>, Context, Error, A, B> accHints(
    hints1: Hints,
    crossinline okContinuation: (A, State<S, Context, Error>, Hints) -> B,
): (A, State<S, Context, Error>, Hints) -> B = { input, state, hints2 ->
    okContinuation(input, state, Hints(hints1.hints + hints2.hints))
}


inline fun refreshHints(
    hints: Hints,
    error: ErrorItem<*>?
) = when {
    error == null -> Hints.empty()
    hints.hints.isEmpty() -> hints
    else -> Hints(setOf(error))
}


inline fun <Tokens : Any, Token : Any, Context, Error, Output> token(
    crossinline test: (Token) -> Output?,
    errorItems: Set<ErrorItem<Token>>
): ParsekT<Stream<Tokens, Token>, Context, Error, Output> =
    object : ParsekT<Stream<Tokens, Token>, Context, Error, Output> {
        override fun <B> invoke(
            state: State<Stream<Tokens, Token>, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<Stream<Tokens, Token>, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<Stream<Tokens, Token>, Context, Error>) -> B,
            emptyOk: (Output, State<Stream<Tokens, Token>, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<Stream<Tokens, Token>, Context, Error>) -> B
        ): B = when (val taken = state.stateInput.uncons()) {
            null ->
                emptyError(
                    ParseError.TrivialError(
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


inline fun <Tokens : Any, Token : Any, Context, Error> tokens(
    crossinline test: (Tokens, Tokens) -> Boolean,
    tokens: Tokens
) =
    object : ParsekT<Stream<Tokens, Token>, Context, Error, Tokens> {
        override fun <B> invoke(
            state: State<Stream<Tokens, Token>, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Tokens, State<Stream<Tokens, Token>, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<Stream<Tokens, Token>, Context, Error>) -> B,
            emptyOk: (Tokens, State<Stream<Tokens, Token>, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<Stream<Tokens, Token>, Context, Error>) -> B
        ): B {
            val unexpected: (pos: Int, errorItem: ErrorItem<Token>) -> ParseError.TrivialError<Token> =
                { pos, errorItem ->
                    ParseError.TrivialError(
                        pos,
                        errorItem,
                        setOf(ErrorItem.Tokens(state.stateInput.chunkToTokens(tokens).toNonEmptyListOrNull()!!))
                    )
                }

            val tokenLength = state.stateInput.chunkLength(tokens)
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

                        if (state.stateInput.chunkEmpty(tokens)) {
                            emptyOk(tts, nextState, Hints.empty())
                        } else {
                            consumedOk(tts, nextState, Hints.empty())
                        }

                    } else {
                        emptyError(
                            unexpected(
                                state.stateOffset,
                                ErrorItem.Tokens(state.stateInput.chunkToTokens(tts).toNonEmptyListOrNull()!!)
                            ),
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

inline fun <S : Stream<*, *>> eof(): Parser<S, Unit> =
    object : Parser<S, Unit> {
        override fun <B> invoke(
            state: State<S, Any, Nothing>,
            trampoline: (() -> B) -> B,
            consumedOk: (Unit, State<S, Any, Nothing>, Hints) -> B,
            consumedError: (ParseError<Nothing>, State<S, Any, Nothing>) -> B,
            emptyOk: (Unit, State<S, Any, Nothing>, Hints) -> B,
            emptyError: (ParseError<Nothing>, State<S, Any, Nothing>) -> B
        ): B = trampoline {
            when (val x = state.stateInput.uncons()) {
                null -> emptyOk(Unit, state, Hints.empty())
                else -> {
                    val us = ErrorItem.Tokens(nonEmptyListOf(x.first))
                    val ps = setOf(ErrorItem.EndOfInput)

                    emptyError(
                        ParseError.TrivialError(state.stateOffset, us, ps),
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


inline fun <Token : Any, Tokens : Any> pTakeWhile(
    ml: String?,
    noinline test: (Token) -> Boolean
): Parser<Stream<Tokens, Token>, Tokens> = object : Parser<Stream<Tokens, Token>, Tokens> {
    override fun <B> invoke(
        state: State<Stream<Tokens, Token>, Any, Nothing>,
        trampoline: (() -> B) -> B,
        consumedOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints) -> B,
        consumedError: (ParseError<Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B,
        emptyOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints) -> B,
        emptyError: (ParseError<Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B
    ): B = trampoline {
        val (ts, nextInput) = state.stateInput.takeWhile(test)
        val len = state.stateInput.chunkLength(ts)
        val hs = Hints(setOfNotNull(ml?.let { ErrorItem.Label(it) }))

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


inline fun <Token : Any, Tokens : Any> pTakeWhile1(
    ml: String?,
    noinline test: (Token) -> Boolean
): Parser<Stream<Tokens, Token>, Tokens> = object : Parser<Stream<Tokens, Token>, Tokens> {
    override fun <B> invoke(
        state: State<Stream<Tokens, Token>, Any, Nothing>,
        trampoline: (() -> B) -> B,
        consumedOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints) -> B,
        consumedError: (ParseError<Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B,
        emptyOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints) -> B,
        emptyError: (ParseError<Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B
    ): B = trampoline {
        val (ts, nextInput) = state.stateInput.takeWhile(test)
        val len = state.stateInput.chunkLength(ts)
        val el = ml?.let { ErrorItem.Label(it) }
        val hs = Hints(setOfNotNull(el))

        when (state.stateInput.chunkEmpty(ts)) {
            true -> {
                val us = when (val t = state.stateInput.uncons()) {
                    null -> ErrorItem.EndOfInput
                    else -> ErrorItem.Tokens(nonEmptyListOf(t.first))
                }

                val ps = setOfNotNull(el)

                emptyError(
                    ParseError.TrivialError(state.stateOffset, us, ps),
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

inline fun <Token : Any, Tokens : Any> pTake(
    ml: String?,
    count: Int
): Parser<Stream<Tokens, Token>, Tokens> = object : Parser<Stream<Tokens, Token>, Tokens> {
    override fun <B> invoke(
        state: State<Stream<Tokens, Token>, Any, Nothing>,
        trampoline: (() -> B) -> B,
        consumedOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints) -> B,
        consumedError: (ParseError<Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B,
        emptyOk: (Tokens, State<Stream<Tokens, Token>, Any, Nothing>, Hints) -> B,
        emptyError: (ParseError<Nothing>, State<Stream<Tokens, Token>, Any, Nothing>) -> B
    ): B = trampoline {
        val n = max(0, count)
        val el = ml?.let { ErrorItem.Label(it) }
        val ps = setOfNotNull(el)

        when (val taken = state.stateInput.takeN(n)) {
            null -> emptyError(
                ParseError.TrivialError(state.stateOffset, ErrorItem.EndOfInput, ps), state
            )

            else -> {
                val (ts, nextInput) = taken
                val len = state.stateInput.chunkLength(ts)
                if (len != count) {
                    emptyError(
                        ParseError.TrivialError(state.stateOffset + len, ErrorItem.EndOfInput, ps),
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


inline fun <S : Stream<*, *>, Error> parseError(error: ParseError<Error>) =
    object : ParserE<S, Error, Nothing> {
        override fun <B> invoke(
            state: State<S, Any, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Nothing, State<S, Any, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Any, Error>) -> B,
            emptyOk: (Nothing, State<S, Any, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Any, Error>) -> B
        ): B = trampoline { emptyError(error, state) }
    }


inline infix fun <S : Stream<*, *>, Context, Error, Output> ParsekT<S, Context, Error, Output>.label(
    label: String
) = object : ParsekT<S, Context, Error, Output> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output, State<S, Context, Error>, Hints) -> B,
        consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output, State<S, Context, Error>, Hints) -> B,
        emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
    ): B = trampoline {
        val el = label.takeIf { it.isNotEmpty() }?.let { ErrorItem.Label(it) }
        this@label(
            state,
            trampoline,
            { a, b, c ->
                when (el) {
                    null -> consumedOk(a, b, refreshHints(c, null))
                    else -> consumedOk(a, b, c)
                }
            },
            consumedError,
            { a, b, c ->
                emptyOk(a, b, refreshHints(c, el))
            },
            { a, b ->
                emptyError(
                    when (a) {
                        is ParseError.TrivialError<*> ->
                            ParseError.TrivialError(
                                a.offset,
                                a.unexpected,
                                setOfNotNull(el)
                            )

                        else -> a
                    },
                    b
                )
            }
        )
    }
}


inline fun <S : Stream<*, *>, Context, Error, Output> ParsekT<S, Context, Error, Output>.withRecovery(
    crossinline recoverBy: (ParseError<Error>) -> ParsekT<S, Context, Error, Output>
) = object : ParsekT<S, Context, Error, Output> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output, State<S, Context, Error>, Hints) -> B,
        consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output, State<S, Context, Error>, Hints) -> B,
        emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
    ): B = trampoline {
        this@withRecovery(
            state,
            trampoline,
            consumedOk,
            { error, s ->
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
            },
            emptyOk,
            { error, s ->
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
            }
        )
    }
}

inline fun <S : Stream<*, *>, Error, Context, Output> ParsekT<S, Context, Error, Output>.observing(): ParsekT<S, Context, Error, Either<ParseError<Error>, Output>> =
    object : ParsekT<S, Context, Error, Either<ParseError<Error>, Output>> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Either<ParseError<Error>, Output>, State<S, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
            emptyOk: (Either<ParseError<Error>, Output>, State<S, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
        ): B = trampoline {
            this@observing(
                state,
                trampoline,
                { a, b, c -> consumedOk(Either.Right(a), b, c) },
                { error, s -> consumedOk(Either.Left(error), s, Hints.empty()) },
                { a, b, c -> emptyOk(Either.Right(a), b, c) },
                { error, s -> emptyOk(Either.Left(error), s, Hints.toHints(s.stateOffset, error)) }
            )
        }
    }


inline fun <S : Any> satisfy(crossinline test: (Char) -> Boolean): Parser<Stream<S, Char>, Char> =
    token(
        test = { if (test(it)) it else null },
        errorItems = emptySet()
    )

inline fun <S : Stream<*, *>, Error, Context, Output> ParsekT<S, Context, Error, Output>.optional(): ParsekT<S, Context, Error, Output?> =
    this or pure(null)

fun <S : Stream<*, *>, Error, Context, Output, End> ParsekT<S, Context, Error, Output>.manyTill(
    end: ParsekT<S, Context, Error, End>
): ParsekT<S, Context, Error, List<Output>> {
//    fun go(f: List<Output>): ParsekT<S, Context, Error, List<Output>> = `do` {
//        val done = !end.optional()
//        when (done) {
//            null -> !go(f + !this@manyTill)
//            else -> f
//        }
//    }

    fun go(f: List<Output>): ParsekT<S, Context, Error, List<Output>> =
        end.optional().bind { done ->
            when (done) {
                null -> this@manyTill.bind { res -> go(f + res) }
                else -> pure(f)
            }
        }

    return go(emptyList())
}

fun <S : Stream<*, *>, Error, Context, Output> ParsekT<S, Context, Error, Output>.many(): ParsekT<S, Context, Error, List<Output>> {
//    fun go(f: List<Output>): ParsekT<S, Context, Error, List<Output>> = `do` {
//        val done = !this@many.optional()
//        when (done) {
//            null -> !go(f + !this@many)
//            else -> f
//        }
//    }

    fun go(f: List<Output>): ParsekT<S, Context, Error, List<Output>> =
        this@many.optional().bind { done ->
            when (done) {
                null -> {
                    pure(f)
                }
                else -> {

                    go(f + done)
                }
            }
        }

    return go(emptyList())
}


inline fun <S : Any> anyChar(): Parser<Stream<S, Char>, Char> = satisfy(constant(true))


inline fun <S : Any> char(char: Char): Parser<Stream<S, Char>, Char> =
    token(
        test = { if (it == char) it else null },
        errorItems = setOf(ErrorItem.Tokens(nonEmptyListOf(char)))
    )

@JvmName("NonGenericChar")
inline fun char(char: Char): Parser<Stream<String, Char>, Char> =
    token(
        test = { if (it == char) it else null },
        errorItems = setOf(ErrorItem.Tokens(nonEmptyListOf(char)))
    )

inline fun space1(): Parser<Stream<String, Char>, Unit> =
    -pTakeWhile1<Char, String>("white space", Char::isWhitespace)

inline fun space(): Parser<Stream<String, Char>, Unit> =
    -pTakeWhile<Char, String>("zero or more white space", Char::isWhitespace)

inline fun string(str: String): Parser<Stream<String, Char>, String> =
    tokens(String::equals, str)

inline fun double(): Parser<Stream<String, Char>, Double> =
    -(char('-').optional()) *
    (pTakeWhile1<Char, String>(
        "digit",
        Char::isDigit
    ).map { it.toInt() } * char('.') * pTakeWhile1<Char, String>(
        "digit",
        Char::isDigit
    ).map { it.toInt() }).attempt().map { (a, _, b) -> "$a.$b".toDouble() } or
            pTakeWhile1<Char, String>(
                "digit",
                Char::isDigit
            ).map { it.toDouble() }.attempt()

fun <Output> Parser<Stream<String, Char>, Output>.lexeme(): Parser<Stream<String, Char>, Output> =
    -(space().optional()) * this@lexeme * -(space().optional())


inline fun <S : Stream<*, *>, Context> getContext(): ParserC<S, Context, Context> =
    object : ParserC<S, Context, Context> {
        override fun <B> invoke(
            state: State<S, Context, Nothing>,
            trampoline: (() -> B) -> B,
            consumedOk: (Context, State<S, Context, Nothing>, Hints) -> B,
            consumedError: (ParseError<Nothing>, State<S, Context, Nothing>) -> B,
            emptyOk: (Context, State<S, Context, Nothing>, Hints) -> B,
            emptyError: (ParseError<Nothing>, State<S, Context, Nothing>) -> B
        ): B = trampoline { emptyOk(state.stateContext.context, state, Hints.empty()) }
    }


inline fun <S : Stream<*, *>, Context, Error, Output> ref(
    parserProperty: KProperty0<ParsekT<S, Context, Error, Output>>
) : ParsekT<S, Context, Error, Output> {
    return object : ParsekT<S, Context, Error, Output> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
            emptyOk: (Output, State<S, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
        ): B = parserProperty()(
            state,
            trampoline,
            consumedOk,
            consumedError,
            emptyOk,
            emptyError
        )
    }
}

inline fun <S : Stream<*, *>, Context, Error> updateContext(crossinline fn: (Context) -> Context): ParsekT<S, Context, Error, Unit> =
    object : ParsekT<S, Context, Error, Unit> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Unit, State<S, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
            emptyOk: (Unit, State<S, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
        ): B = trampoline {
            emptyOk(
                Unit,
                state.copy(stateContext = state.stateContext.copy(fn(state.stateContext.context))),
                Hints.empty()
            )
        }
    }


inline fun <S : Stream<*, *>, Context, Error, Output> pure(pure: Output): ParsekT<S, Context, Error, Output> =
    object : ParsekT<S, Context, Error, Output> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
            emptyOk: (Output, State<S, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
        ): B = trampoline { emptyOk(pure, state, Hints.empty()) }
    }


inline infix fun <S : Stream<*, *>, Context, Error, Output1, Output2> ParsekT<S, Context, Error, Output1>.map(
    crossinline fn: (Output1) -> Output2
) = object : ParsekT<S, Context, Error, Output2> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output2, State<S, Context, Error>, Hints) -> B,
        consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output2, State<S, Context, Error>, Hints) -> B,
        emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
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

inline infix fun <S : Stream<*, *>, Context, Error, Output1> ParsekT<S, Context, Error, Output1>.or(
    or: ParsekT<S, Context, Error, Output1>
): ParsekT<S, Context, Error, Output1> = alt(or)

inline fun <S : Stream<*, *>, Context, Error, Output1> ParsekT<S, Context, Error, Output1>.alt(
    or: ParsekT<S, Context, Error, Output1>
): ParsekT<S, Context, Error, Output1> = object : ParsekT<S, Context, Error, Output1> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output1, State<S, Context, Error>, Hints) -> B,
        consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output1, State<S, Context, Error>, Hints) -> B,
        emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
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


inline fun <S : Stream<*, *>, Context, Error, Output1, Output2> ParsekT<S, Context, Error, Output1>.flatMap(
    crossinline fn: (Output1) -> ParsekT<S, Context, Error, Output2>
): ParsekT<S, Context, Error, Output2> = bind(fn)


inline fun <S : Stream<*, *>, Context, Error, Output1, Output2> ParsekT<S, Context, Error, Output1>.bind(
    crossinline cont: (Output1) -> ParsekT<S, Context, Error, Output2>
): ParsekT<S, Context, Error, Output2> = object : ParsekT<S, Context, Error, Output2> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (Output2, State<S, Context, Error>, Hints) -> B,
        consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
        emptyOk: (Output2, State<S, Context, Error>, Hints) -> B,
        emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
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

internal inline fun <S : Stream<*, *>, Context, Error, Output> zero(): ParsekT<S, Context, Error, Output> =
    object : ParsekT<S, Context, Error, Output> {
        override fun <B> invoke(
            state: State<S, Context, Error>,
            trampoline: (() -> B) -> B,
            consumedOk: (Output, State<S, Context, Error>, Hints) -> B,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
            emptyOk: (Output, State<S, Context, Error>, Hints) -> B,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
        ): B = emptyError(ParseError.TrivialError<Any>(state.stateOffset, null, emptySet()), state)
    }

@JvmName("timesPair")
inline operator fun <S : Stream<*, *>, Context, Error, A, B1> ParsekT<S, Context, Error, A>.times(
    b: ParsekT<S, Context, Error, B1>
): ParsekT<S, Context, Error, Pair<A, B1>> = ap(fp = ap(fp = pure { a: A -> { b: B1 -> Pair(a, b) } }, this), b)

@JvmName("timesUnitThis")
inline operator fun <S : Stream<*, *>, Context, Error, B1> ParsekT<S, Context, Error, Unit>.times(
    b: ParsekT<S, Context, Error, B1>
): ParsekT<S, Context, Error, B1> = ap(fp = ap(fp = pure { _ -> { b: B1 -> b } }, this), b)

@JvmName("timesUnitOther")
inline operator fun <S : Stream<*, *>, Context, Error, A> ParsekT<S, Context, Error, A>.times(
    b: ParsekT<S, Context, Error, Unit>
): ParsekT<S, Context, Error, A> = ap(fp = ap(fp = pure { a: A -> { a } }, this), b)

@JvmName("timesTriple")
inline operator fun <S : Stream<*, *>, Context, Error, A, B1, C> ParsekT<S, Context, Error, Pair<A, B1>>.times(
    b: ParsekT<S, Context, Error, C>
): ParsekT<S, Context, Error, Triple<A, B1, C>> =
    ap(fp = ap(fp = pure { a: Pair<A, B1> -> { b: C -> a + b } }, this), b)

@JvmName("timesPairUnitOther")
inline operator fun <S : Stream<*, *>, Context, Error, A, B1> ParsekT<S, Context, Error, Pair<A, B1>>.times(
    b: ParsekT<S, Context, Error, Unit>
): ParsekT<S, Context, Error, Pair<A, B1>> =
    ap(fp = ap(fp = pure { a: Pair<A, B1> -> { _ -> a } }, this), b)

@JvmName("timesTuple4")
inline operator fun <S : Stream<*, *>, Context, Error, A, B1, C, D> ParsekT<S, Context, Error, Triple<A, B1, C>>.times(
    b: ParsekT<S, Context, Error, D>
): ParsekT<S, Context, Error, Tuple4<A, B1, C, D>> =
    ap(fp = ap(fp = pure { a: Triple<A, B1, C> -> { b: D -> a + b } }, this), b)

@JvmName("timesTripleUnitOther")
inline operator fun <S : Stream<*, *>, Context, Error, A, B1, C> ParsekT<S, Context, Error, Triple<A, B1, C>>.times(
    b: ParsekT<S, Context, Error, Unit>
): ParsekT<S, Context, Error, Triple<A, B1, C>> =
    ap(fp = ap(fp = pure { a: Triple<A, B1, C> -> { _ -> a } }, this), b)

@JvmName("timesTuple5")
inline operator fun <S : Stream<*, *>, Context, Error, A, B1, C, D, E> ParsekT<S, Context, Error, Tuple4<A, B1, C, D>>.times(
    b: ParsekT<S, Context, Error, E>
): ParsekT<S, Context, Error, Tuple5<A, B1, C, D, E>> =
    ap(fp = ap(fp = pure { a: Tuple4<A, B1, C, D> -> { b: E -> a + b } }, this), b)

@JvmName("timesTuple4UnitOther")
inline operator fun <S : Stream<*, *>, Context, Error, A, B1, C, D> ParsekT<S, Context, Error, Tuple4<A, B1, C, D>>.times(
    b: ParsekT<S, Context, Error, Unit>
): ParsekT<S, Context, Error, Tuple4<A, B1, C, D>> =
    ap(fp = ap(fp = pure { a: Tuple4<A, B1, C, D> -> { _ -> a } }, this), b)


inline operator fun <S : Stream<*, *>, Context, Error, A> ParsekT<S, Context, Error, A>.unaryMinus(
): ParsekT<S, Context, Error, Unit> = ap(fp = pure { }, a = this)

inline fun <S : Stream<*, *>, Context, Error, A, B1> ap(
    fp: ParsekT<S, Context, Error, (A) -> B1>,
    a: ParsekT<S, Context, Error, A>
): ParsekT<S, Context, Error, B1> = object : ParsekT<S, Context, Error, B1> {
    override fun <B> invoke(
        state: State<S, Context, Error>,
        trampoline: (() -> B) -> B,
        consumedOk: (B1, State<S, Context, Error>, Hints) -> B,
        consumedError: (ParseError<Error>, State<S, Context, Error>) -> B,
        emptyOk: (B1, State<S, Context, Error>, Hints) -> B,
        emptyError: (ParseError<Error>, State<S, Context, Error>) -> B
    ): B = trampoline {
        fp(
            state,
            trampoline,
            { x, s, hs ->
                a(
                    s,
                    trampoline,
                    consumedOk.curried().compose(x).uncurried(),
                    consumedError,
                    accHints(hs, consumedOk.curried().compose(x).uncurried()),
                    withHints(hs, consumedError)
                )
            },
            consumedError,
            { x, s, hs ->
                a(
                    s,
                    trampoline,
                    consumedOk.curried().compose(x).uncurried(),
                    consumedError,
                    accHints(hs, emptyOk.curried().compose(x).uncurried()),
                    withHints(hs, emptyError)
                )
            },
            emptyError
        )
    }
}

fun <S : Stream<*, *>, Context, Error, A, Open, Close> ParsekT<S, Context, Error, A>.between(
    open: ParsekT<S, Context, Error, Open>,
    close: ParsekT<S, Context, Error, Close>
): ParsekT<S, Context, Error, A> = -open * this@between * -close


fun <S : Stream<*, *>, Context, Error, A> ParsekT<S, Context, Error, A>.skipMany(): ParsekT<S, Context, Error, Unit> =
    -(this@skipMany.many())

fun <S : Stream<*, *>, Context, Error, A> choice(
    vararg cs: ParsekT<S, Context, Error, A>
): ParsekT<S, Context, Error, A> =
    cs.foldRight(zero()) { a, b -> a or b }


fun <S : Stream<*, *>, Context, Error, A, B, C> liftA2(
    f: (A, B) -> C,
    a: ParsekT<S, Context, Error, A>,
    b: ParsekT<S, Context, Error, B>
): ParsekT<S, Context, Error, C> = (a * b) map { (a, b) -> f(a, b) }

fun <S : Stream<*, *>, Context, Error, A, Sep> ParsekT<S, Context, Error, A>.sepBy(
    sep: ParsekT<S, Context, Error, Sep>
): ParsekT<S, Context, Error, List<A>> = this@sepBy.sepBy1(sep) or pure<S, Context, Error, List<A>>(emptyList()).label("empty")

inline fun <S : Stream<*, *>, Context, Error, A, Sep> ParsekT<S, Context, Error, A>.sepBy1(
    sep: ParsekT<S, Context, Error, Sep>
): ParsekT<S, Context, Error, List<A>> =
    (this@sepBy1 * (-sep * this@sepBy1).many()).map { (a, b) -> listOf(a) + b }


typealias InitRec<Stream, Context, Error, Output> =
            () -> Trampoline<Stream, Context, Error, Output>

typealias StepRec<Stream, Context, Error, Output> =
        Step<InitRec<Stream, Context, Error, Output>, Reply<Stream, Context, Error, Output>>

typealias RunRec<Stream, Context, Error, Output> =
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

    fun toBundle(errors: NonEmptyList<ParseError<Error>>) =
        ParseErrorBundle(
            bundleErrors = errors.sortedBy { it.offset }.toNonEmptyListOrNull()!!,
            bundlePosState = reply.state.statePosState
        )

    return when (reply.result) {
        is Result.Error -> Either.Left(toBundle(nonEmptyListOf(reply.result.error)))
        is Result.Ok -> Either.Right(reply.result.result)
    }
}

