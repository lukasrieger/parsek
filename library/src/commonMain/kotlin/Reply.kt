import error.ErrorItem
import error.ParseError
import stream.Stream

data class Reply<S : Stream<*, *>, C, E, A>(
    val state: State<S, C, E>,
    val consumption: Consumption,
    val result: Result<E, A>
)


enum class Consumption {

    /**
     * Some part of input stream was consumed
     */
    Consumed,

    /**
     * No input was consumed
     */
    NotConsumed
}


data class Hints<out T>(val hints: Set<ErrorItem<T>>) {
    companion object {
        fun empty(): Hints<Nothing> = Hints(hints = emptySet())

        fun <S, E> toHints(pos: Int, error: ParseError<S, E>) = when (error) {
            is ParseError.TrivialError<*, *> -> if (pos == error.offset) {
                Hints(error.expected.takeIf { it.isNotEmpty() } ?: emptySet())
            } else {
                empty()
            }

            else -> empty()
        }
    }

    operator fun plus(other: Hints<@UnsafeVariance T>): Hints<T> = Hints(hints + other.hints)
}


sealed interface Result<out E, out A> {

    data class Ok<out A>(val hints: Hints<*>, val result: A) : Result<Nothing, A>


    data class Error<out S, out E>(val error: ParseError<S, E>) : Result<E, Nothing>

}

