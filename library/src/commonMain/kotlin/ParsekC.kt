import Repl.*
import error.ParseError
import error.plus
import util.Ordering
import util.compare

data class PosStateC(
    /**
     * The rest of input to process
     */
    val pStateInput: String,
    /**
     * Offset corresponding to beginning of [pStateInput]
     */
    val pStateOffset: Int,
    /**
     * Source position corresponding to beginning of [pStateInput]
     */
    val pStateSourcePos: SourcePos,
    /**
     * Tab width to use for column calculation
     */
    val pStateTabWidth: Pos,
    /**
     * Prefix to prepend to offending line
     */
    val pStateLinePrefix: String
) {
    companion object {
        fun initial(name: FilePath, input: String): PosStateC = PosStateC(
            pStateInput = input,
            pStateOffset = 0,
            pStateSourcePos = SourcePos.initial(name),
            pStateTabWidth = Pos.DefaultTabWidth,
            pStateLinePrefix = ""
        )
    }
}


data class StateC(
    /**
     * The rest input to process
     */
    val stateInput: String,

    /**
     * The number of processed tokens so far
     */
    val stateOffset: Int,
    /**
     * State that is used for line/column calculation
     */
    val statePosState: PosStateC,
)

infix fun StateC.longestMatch(other: StateC): StateC =
    when (this.stateOffset compare other.stateOffset) {
        is Ordering.LT -> other
        is Ordering.EQ -> other
        is Ordering.GT -> this
    }

sealed interface Repl<R, out E, out O> {

    val state: StateC

    val environment: Env<R>

    data class ConsumedOk<R, out E, out O>(
        override val state: StateC,
        val hints: Hints,
        val result: O,
        override val environment: Env<R>
    ) : Repl<R, E, O>

    data class ConsumedError<R, out E>(
        override val state: StateC,
        val error: ParseError<E>,
        override val environment: Env<R>
    ) : Repl<R, E, Nothing>

    data class EmptyOk<R, out E, out O>(
        override val state: StateC,
        val hints: Hints,
        val result: O,
        override val environment: Env<R>
    ) : Repl<R, E, O>

    data class EmptyError<R, out E>(
        override val state: StateC,
        val error: ParseError<E>,
        override val environment: Env<R>
    ) : Repl<R, E, Nothing>
}


typealias ParserO<O> = ParsekC<Any, Nothing, O>

interface Env<out R> {
    val v: R

    fun get(): R

    fun <R1> set(value: R1): Env<R> where R1 : @UnsafeVariance R
}

fun interface ParsekC<in R, out E, out O> {
    operator fun invoke(env: Env<R>, state: StateC): Repl<in R, E, O>
}

inline fun <O> pureC(pure: O): ParserO<O> = ParsekC { env, state ->
    EmptyOk(
        state = state,
        hints = Hints.empty(),
        result = pure,
        environment = env
    )
}

inline fun <R> environment(): ParsekC<R, Nothing, R> = ParsekC { env, state ->
    EmptyOk(
        state = state,
        hints = Hints.empty(),
        result = env.get(),
        environment = env
    )
}

inline fun <R> modifyEnvironment(crossinline transform: (R) -> R): ParsekC<R, Nothing, Unit> = ParsekC { env, state ->
    EmptyOk(
        state = state,
        hints = Hints.empty(),
        result = Unit,
        environment = env.set(transform(env.get()))
    )
}


inline fun <R, E, O1, O2> ParsekC<R, E, O1>.map(
    crossinline transform: (O1) -> O2
): ParsekC<R, E, O2> = ParsekC { env, state ->
    when (val reply = this@map(env, state)) {
        is ConsumedOk -> ConsumedOk(
            state = reply.state,
            hints = reply.hints,
            result = transform(reply.result),
            environment = env
        )
        is EmptyOk -> EmptyOk(
            state = reply.state,
            hints = reply.hints,
            result = transform(reply.result),
            environment = env
        )
        is ConsumedError -> reply
        is EmptyError -> reply
    }
}


inline fun <R, E, O1, O2> ParsekC<R, E, O1>.bind(
    crossinline transform: (O1) -> ParsekC<R, E, O2>
): ParsekC<R, E, O2> = ParsekC { env, state ->

    val z: Repl<in R, E, O2> = when (val reply: Repl<in R, E, O1> = this@bind(env, state)) {
        is ConsumedOk -> transform(reply.result)(env, state)
        is EmptyOk -> transform(reply.result)(env, state)
        is ConsumedError -> reply
        is EmptyError -> reply
    }

    return@ParsekC z
}


inline infix fun <R, E, O> ParsekC<R, E, O>.or(
    other: ParsekC<R, E, O>
): ParsekC<R, E, O> = ParsekC { env, state ->
    when (val reply = this@or(env, state)) {
        is EmptyError -> when (val next = other(env, state)) {
            is ConsumedOk -> next
            is ConsumedError -> ConsumedError(
                state = reply.state longestMatch next.state,
                error = reply.error + next.error,
                environment = env
            )

            is EmptyError -> EmptyError(
                state = reply.state longestMatch next.state,
                error = reply.error + next.error,
                environment = env
            )

            is EmptyOk -> EmptyOk(
                state = next.state,
                hints = Hints.toHints(next.state.stateOffset, reply.error) + next.hints,
                result = next.result,
                environment = env
            )
        }

        else -> reply
    }
}

val p1 = pureC("Hello World!")
val p2 = pureC("Bye World!")

val p4: ParsekC<Any, Throwable, String> = p2

val p3 = p1 or p4

val p9: ParsekC<Number, Throwable, String> = TODO()
val p8: ParsekC<Int, Throwable, String> = TODO()

val p7 = p9 or p8


val p5 = environment<String>().bind {
    println(it)
    modifyEnvironment { "Bye World!" }
}