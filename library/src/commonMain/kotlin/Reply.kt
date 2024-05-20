


data class Reply<E, C, A>(
    val state: State<E, C>,
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
        fun <T> empty(): Hints<T> = Hints(hints = emptySet())
    }
}


sealed interface Result<out E, out A> {

    data class Ok<A>(val hints: Hints<Char>, val result: A) : Result<Nothing, A>


    data class Error<E>(val error: ParseError<E>) : Result<E, Nothing>

}

