data class Reply<S : Stream<*, *>, E, C, A>(
    val state: State<S, E, C>,
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


data class Hints<T>(val hints: Set<ErrorItem<T>>) {
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
}


sealed interface Result<out E, out A> {

    data class Ok<A>(val hints: Hints<*>, val result: A) : Result<Nothing, A>


    data class Error<S, E>(val error: ParseError<S, E>) : Result<E, Nothing>

}

