import error.ErrorItem
import error.ParseError

sealed interface Result<out E, out A> {

    data class Ok<out A>(val hints: Hints, val result: A) : Result<Nothing, A>

    data class Error<out E>(val error: ParseError<E>) : Result<E, Nothing>

}


data class Hints(val hints: Set<ErrorItem<*>>) {
    companion object {
        fun empty(): Hints = Hints(hints = emptySet())

        fun <E> toHints(pos: Int, error: ParseError<E>) = when (error) {
            is ParseError.TrivialError<*> -> if (pos == error.offset) {
                Hints(error.expected.takeIf { it.isNotEmpty() } ?: emptySet())
            } else {
                empty()
            }
            else -> empty()
        }
    }
}

internal operator fun Hints.plus(other: Hints): Hints =
    Hints(hints + other.hints)