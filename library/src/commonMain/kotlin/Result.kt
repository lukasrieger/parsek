import error.ErrorItem
import error.ParseError

sealed interface Result<out E, out A> {

    data class Ok<out A>(val hints: Hints<*>, val result: A) : Result<Nothing, A>

    data class Error<out S, out E>(val error: ParseError<S, E>) : Result<E, Nothing>

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