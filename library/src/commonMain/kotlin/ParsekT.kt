import kotlin.jvm.JvmInline


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



data class Reply<E, C, A>(
    val state: State<E, C>,
    val consumption: Consumption,
    val result: Result<E, A>
)

data class ContextState<Context>(val context: Context)


data class State<out E, C>(
    /**
     * The rest input to process
     */
    val stateInput: String,

    val stateContext: ContextState<C>,
    /**
     * The number of processed tokens so far
     */
    val stateOffset: Int,
    /**
     * State that is used for line/column calculation
     */
    val statePosState: PosState,
    /**
     * Collection of "delayed" [ParseError]'s in reverse order.
     * This means that the last error is the first element of the list.
     */
    val stateParseErrors: List<ParseError<E>>
)





sealed interface Result<out E, out A> {

    data class Ok<A>(val hints: Hints<Char>, val result: A) : Result<Nothing, A>


    data class Error<E>(val error: ParseError<E>) : Result<E, Nothing>

}


