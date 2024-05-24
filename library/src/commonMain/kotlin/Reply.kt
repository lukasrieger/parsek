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



