import arrow.core.nonEmptyListOf
import arrow.core.toNonEmptyListOrNull
import stream.take1
import stream.takeN


interface ParsekT<Error, out Context, out Output> {

    context(Context)
    fun <B> unparser(
        state: State<Error>,
        consumedOk: context(Context) (Output, State<Error>, Hints<Char>) -> B,
        consumedError: context(Context) (ParseError<Error>, State<Error>) -> B,
        emptyOk: context(Context) (Output, State<Error>, Hints<Char>) -> B,
        emptyError: context(Context) (ParseError<Error>, State<Error>) -> B
    ): B
}

typealias ParsekC<Error, Context, Output, B> = context(Context) (
    state: State<Error>,
    consumedOk: ConsumedOk<Error, Context, Output, B>,
    consumedError: ConsumedError<Error, Context, B>,
    emptyOk: EmptyOk<Error, Context, Output, B>,
    emptyError: EmptyError<Error, Context, B>
) -> B

typealias ConsumedOk<E, C, O, B> = context(C) (O, State<E>, Hints<Char>) -> B
typealias ConsumedError<E, C, B> = context(C) (ParseError<E>, State<E>) -> B
typealias EmptyOk<E, C, O, B> = context(C) (O, State<E>, Hints<Char>) -> B
typealias EmptyError<E, C, B> = context(C) (ParseError<E>, State<E>) -> B


fun <E, C, O> fail(message: String): ParsekT<E, C, O> =
    object : ParsekT<E, C, O> {
        context(C) override fun <B> unparser(
            state: State<E>,
            consumedOk: ConsumedOk<E, C, O, B>,
            consumedError: ConsumedError<E, C, B>,
            emptyOk: EmptyOk<E, C, O, B>,
            emptyError: EmptyError<E, C, B>
        ): B = emptyError(
            this@C,
            ParseError.FancyError(state.stateOffset, setOf(ErrorFancy.ErrorFail(message))),
            state
        )
    }

fun <E, C, O> ParsekT<E, C, O>.attempt() =
    object : ParsekT<E, C, O> {
        context(C) override fun <B> unparser(
            state: State<E>,
            consumedOk: ConsumedOk<E, C, O, B>,
            consumedError: ConsumedError<E, C, B>,
            emptyOk: EmptyOk<E, C, O, B>,
            emptyError: EmptyError<E, C, B>
        ): B = this@attempt.unparser(
            state,
            consumedOk,
            { err, _ -> emptyError(this@C, err, state) },
            emptyOk,
            { err, _ -> emptyError(this@C, err, state) }
        )
    }


fun <E, C, O> token(
    test: (Char) -> O?,
    errorItems: Set<ErrorItem<Char>>
): ParsekT<E, C, O> = object : ParsekT<E, C, O> {
    context(C) override fun <B> unparser(
        state: State<E>,
        consumedOk: ConsumedOk<E, C, O, B>,
        consumedError: ConsumedError<E, C, B>,
        emptyOk: EmptyOk<E, C, O, B>,
        emptyError: EmptyError<E, C, B>
    ): B = when (val taken = state.stateInput.take1()) {
        null ->
            emptyError(this@C, ParseError.TrivialError(state.stateOffset, ErrorItem.EndOfInput, errorItems), state)

        else -> {
            val (current, tail) = taken

            when (val res = test(current)) {
                null ->
                    emptyError(
                        this@C,
                        ParseError.TrivialError(
                            state.stateOffset,
                            ErrorItem.Tokens(nonEmptyListOf(current)),
                            errorItems
                        ),
                        State(state.stateInput, state.stateOffset, state.statePosState, state.stateParseErrors)
                    )

                else ->
                    consumedOk(
                        this@C,
                        res,
                        State(tail, state.stateOffset + 1, state.statePosState, state.stateParseErrors),
                        Hints.empty()
                    )
            }
        }
    }
}


fun <E, C> tokens(
    test: (String, String) -> Boolean,
    tokens: String
) = object : ParsekT<E, C, String> {
    context(C) override fun <B> unparser(
        state: State<E>,
        consumedOk: ConsumedOk<E, C, String, B>,
        consumedError: ConsumedError<E, C, B>,
        emptyOk: EmptyOk<E, C, String, B>,
        emptyError: EmptyError<E, C, B>
    ): B {
        val unexpected: (Int, ErrorItem<Char>) -> ParseError<E> = { pos, u ->
            ParseError.TrivialError(
                pos, u, setOf(ErrorItem.Tokens(tokens.toList().toNonEmptyListOrNull()!!))
            )
        }

        return when (val taken = state.stateInput.takeN()) {
            null ->
                emptyError(this@C, unexpected(state.stateOffset, ErrorItem.EndOfInput), state)

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
                        emptyOk(this@C, tts, nextState, Hints.empty())
                    } else {
                        consumedOk(this@C, tts, nextState, Hints.empty())
                    }
                } else {
                    emptyError(
                        this@C,
                        unexpected(state.stateOffset, ErrorItem.Tokens(tts.toList().toNonEmptyListOrNull()!!)),
                        State(state.stateInput, state.stateOffset, state.statePosState, state.stateParseErrors)
                    )
                }
            }
        }
    }
}


fun <E, C, O> pure(it: O) = object : ParsekT<E, C, O> {
    context(C) override fun <B> unparser(
        state: State<E>,
        consumedOk: ConsumedOk<E, C, O, B>,
        consumedError: ConsumedError<E, C, B>,
        emptyOk: EmptyOk<E, C, O, B>,
        emptyError: EmptyError<E, C, B>
    ): B = emptyOk(this@C, it, state, Hints.empty())
}


fun <E, C, O1, O2> ParsekT<E, C, O1>.bind(
    cont: (O1) -> ParsekT<E, C, O2>
): ParsekT<E, C, O2> = object : ParsekT<E, C, O2> {
    context(C) override fun <B> unparser(
        state: State<E>,
        consumedOk: context(C) (O2, State<E>, Hints<Char>) -> B,
        consumedError: context(C) (ParseError<E>, State<E>) -> B,
        emptyOk: context(C) (O2, State<E>, Hints<Char>) -> B,
        emptyError: context(C) (ParseError<E>, State<E>) -> B
    ): B = this@bind.unparser(
        state,
        { x, s, hs ->
            cont(x).unparser(
            s,
            consumedOk,
            consumedError,
            emptyOk,
            emptyError
        ) },
        consumedError,
        { x, s, hs ->
            cont(x).unparser(
                s,
                consumedOk,
                consumedError,
                emptyOk,
                emptyError
            )
        },
        emptyError
    )
}

context(C)
fun <E, C, O> ParsekT<E, C, O>.runParsekT(
    initialState: State<E>,
): Reply<E, O> =
    unparser(
        state = initialState,
        consumedOk = { a, s, hs -> Reply(s, Consumption.Consumed, Result.Ok(hs, a)) },
        consumedError = { err, s -> Reply(s, Consumption.Consumed, Result.Error(err)) },
        emptyOk = { a, s, hs -> Reply(s, Consumption.NotConsumed, Result.Ok(hs, a)) },
        emptyError = { err, s -> Reply(s, Consumption.NotConsumed, Result.Error(err)) }
    )
