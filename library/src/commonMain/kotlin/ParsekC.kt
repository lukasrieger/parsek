import Repl.*
import arrow.core.Tuple4
import arrow.core.Tuple5
import arrow.core.plus
import error.ParseError
import error.plus
import util.Ordering
import util.compare
import kotlin.jvm.JvmName

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
) {
    companion object {
        fun  initial(input: String, name: FilePath): StateC = StateC(
            stateInput = input,
            stateOffset = 0,
            statePosState = PosStateC.initial(name, input),
        )
    }
}

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

fun <R> envOf(e: R): Env<R> = object : Env<R> {
    override val v: R = e
    override fun get(): R = v
    override fun <R1 : R> set(value: R1): Env<R> = envOf(value)
}

typealias ParsekC<R, E, O> = DeepRecursiveFunction<in Pair<Env<R>, StateC>, out Repl<in R, E, O>>

fun <R, E, O> parsekC(
    block: suspend DeepRecursiveScope<in Pair<Env<R>, StateC>, out Repl<in R, E, O>>.(Pair<Env<R>, StateC>) -> Repl<in R, E, O>
): ParsekC<R, E, O> = DeepRecursiveFunction(block)


fun <O> pureC(pure: O): ParserO<O> = parsekC { (env, state) ->
    EmptyOk(
        state = state,
        hints = Hints.empty(),
        result = pure,
        environment = env
    )
}

fun <R, E, O> pureCC(pure: O): ParsekC<R, E, O> = parsekC { (env, state) ->
    EmptyOk(
        state = state,
        hints = Hints.empty(),
        result = pure,
        environment = env
    )
}

fun <R> environment(): ParsekC<R, Nothing, R> = parsekC { (env, state) ->
    EmptyOk(
        state = state,
        hints = Hints.empty(),
        result = env.get(),
        environment = env
    )
}

inline fun <R> modifyEnvironment(crossinline transform: (R) -> R): ParsekC<R, Nothing, Unit> = parsekC { (env, state) ->
    EmptyOk(
        state = state,
        hints = Hints.empty(),
        result = Unit,
        environment = env.set(transform(env.get()))
    )
}


inline fun <R, E, O1, O2> ParsekC<R, E, O1>.map(
    crossinline transform: (O1) -> O2
): ParsekC<R, E, O2> = parsekC { (env, state) ->
    when (val reply = this@map.callRecursive(env to state)) {
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
): ParsekC<R, E, O2> = parsekC { (env, state) ->
    when (val reply: Repl<in R, E, O1> = this@bind.callRecursive(env to state)) {
        is ConsumedOk -> transform(reply.result).callRecursive(env to state)
        is EmptyOk -> transform(reply.result).callRecursive(env to state)
        is ConsumedError -> reply
        is EmptyError -> reply
    }
}


 infix fun <R, E, O> ParsekC<R, E, O>.or(
     other: ParsekC<R, E, O>
): ParsekC<R, E, O> = parsekC { (env, state) ->
    when (val reply = this@or.callRecursive(env to state)) {
        is EmptyError -> when (val next = other.callRecursive(env to state)) {
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

 operator fun <R, E, A, B> ParsekC<R, E, A>.times(
    other: ParsekC<R, E, B>
): ParsekC<R, E, Pair<A, B>> = ap(fp = ap(fp = pureCC { a: A -> { b: B -> Pair(a, b) } }, this), other)

@JvmName("timesUnitThis")
 operator fun <R, E, B1> ParsekC<R, E, Unit>.times(
    b: ParsekC< R, E, B1>
): ParsekC< R, E, B1> = ap(fp = ap(fp = pureCC { _ -> { b: B1 -> b } }, this), b)

@JvmName("timesUnitOther")
 operator fun <R, Error, A> ParsekC<R, Error, A>.times(
     b: ParsekC<R, Error, Unit>
): ParsekC<R, Error, A> = ap(fp = ap(fp = pureCC { a: A -> { a } }, this), b)

@JvmName("timesTriple")
 operator fun <R, Error, A, B1, C> ParsekC<R, Error, Pair<A, B1>>.times(
     b: ParsekC<R, Error, C>
): ParsekC<R, Error, Triple<A, B1, C>> =
    ap(fp = ap(fp = pureCC { a: Pair<A, B1> -> { b: C -> a + b } }, this), b)

@JvmName("timesPairUnitOther")
 operator fun <R, Error, A, B1> ParsekC<R, Error, Pair<A, B1>>.times(
     b: ParsekC<R, Error, Unit>
): ParsekC<R, Error, Pair<A, B1>> =
    ap(fp = ap(fp = pureCC { a: Pair<A, B1> -> { _ -> a } }, this), b)

@JvmName("timesTuple4")
 operator fun <R, Error, A, B1, C, D> ParsekC<R, Error, Triple<A, B1, C>>.times(
     b: ParsekC<R, Error, D>
): ParsekC<R, Error, Tuple4<A, B1, C, D>> =
    ap(fp = ap(fp = pureCC { a: Triple<A, B1, C> -> { b: D -> a + b } }, this), b)

@JvmName("timesTripleUnitOther")
 operator fun <R, Error, A, B1, C> ParsekC<R, Error, Triple<A, B1, C>>.times(
     b: ParsekC<R, Error, Unit>
): ParsekC<R, Error, Triple<A, B1, C>> =
    ap(fp = ap(fp = pureCC { a: Triple<A, B1, C> -> { _ -> a } }, this), b)

@JvmName("timesTuple5")
 operator fun <R, Error, A, B1, C, D, E> ParsekC<R, Error, Tuple4<A, B1, C, D>>.times(
     b: ParsekC<R, Error, E>
): ParsekC<R, Error, Tuple5<A, B1, C, D, E>> =
    ap(fp = ap(fp = pureCC { a: Tuple4<A, B1, C, D> -> { b: E -> a + b } }, this), b)

@JvmName("timesTuple4UnitOther")
operator fun <R, Error, A, B1, C, D> ParsekC<R, Error, Tuple4<A, B1, C, D>>.times(
     b: ParsekC<R, Error, Unit>
): ParsekC<R, Error, Tuple4<A, B1, C, D>> =
    ap(fp = ap(fp = pureCC { a: Tuple4<A, B1, C, D> -> { _ -> a } }, this), b)

fun <R, E, A, B> ap(
     fp: ParsekC<R, E, (A) -> B>,
     a: ParsekC<R, E, A>
): ParsekC<R, E, B> = parsekC { (env, state) ->
    when(val result = fp.callRecursive(env to state)) {
        is ConsumedError -> ConsumedError(result.state, result.error, env)
        is ConsumedOk -> when(val result2 = a.callRecursive(env to result.state)) {
            is ConsumedError -> ConsumedError(result2.state, result2.error, env)
            is ConsumedOk -> ConsumedOk(result2.state, result2.hints, result.result(result2.result), env)
            is EmptyError -> EmptyError(
                result2.state,
                when(result2.error) {
                    is ParseError.TrivialError<*> ->
                        result2.error.copy(
                            expected = result2.error.expected + result.hints.hints
                        )
                    else -> result2.error
                },
                env
            )
            is EmptyOk -> EmptyOk(result2.state, result.hints + result2.hints, result.result(result2.result), env)
        }
        is EmptyError -> EmptyError(result.state, result.error, env)
        is EmptyOk -> when(val result2 = a.callRecursive(env to result.state)) {
            is ConsumedError -> ConsumedError(result2.state, result2.error, env)
            is ConsumedOk -> ConsumedOk(result2.state, result2.hints, result.result(result2.result), env)
            is EmptyError -> EmptyError(
                result2.state,
                when(result2.error) {
                    is ParseError.TrivialError<*> ->
                        result2.error.copy(
                            expected = result2.error.expected + result.hints.hints
                   )
                    else -> result2.error
                },
                env
            )
            is EmptyOk -> EmptyOk(result2.state, result.hints + result2.hints, result.result(result2.result), env)
        }
    }
}

fun <R, E, O> ParsekC<R, E, O>.runParser(
    input: String,
    context: R,
    name: FilePath = FilePath.empty(),
    state: StateC = StateC.initial(input, name)
): Repl<in R, E, O> = this@runParser(envOf(context) to state)

val p1 = pureC("Hello World!")
val p2 = pureC("Bye World!")

val pp3 = p1 * p2 * p2 * p2

val p4: ParsekC<Any, Throwable, String> = p2

val p3 = p1 or p4


val p5 = environment<String>().bind {
    println(it)
    modifyEnvironment { "Bye World!" }
}

val pp5 = p1 * p5


val monadicTest = doParser {
    val first = !p1
    val second = !p4

    first + second
}


var counter = 0

val recursionTest: ParsekC<Any, Nothing, Nothing> = p1.bind<Any, Nothing, String, Nothing> {
    if (counter > 9_000_000_0) error("Counter: $counter")
    counter++
    recursionTest
}

tailrec fun recursionTest2() {
    if (counter > 9_000_000_0) error("Counter: $counter")
    counter++
    recursionTest2()
}

fun main() {
//    recursionTest.runParser(
//        input = "Test",
//        context = "",
//        name = FilePath.empty(),
//    )
    recursionTest2()
}
