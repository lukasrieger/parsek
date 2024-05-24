import error.ParseError
import stream.Stream
import util.Ordering
import util.compare

data class ContextState<out Context>(val context: Context)


data class State<S : Stream<*, *>, out C, E>(
    /**
     * The rest input to process
     */
    val stateInput: S,

    val stateContext: ContextState<C>,
    /**
     * The number of processed tokens so far
     */
    val stateOffset: Int,
    /**
     * State that is used for line/column calculation
     */
    val statePosState: PosState<S>,
    /**
     * Collection of "delayed" [ParseError]'s in reverse order.
     * This means that the last error is the first element of the list.
     */
    val stateParseErrors: List<ParseError<S, E>>

) {
    companion object {
        fun <S : Stream<*, *>, Context, Error> initial(
            name: FilePath,
            input: S,
            context: Context
        ): State<S, Context, Error> = State(
            stateInput = input,
            stateContext = ContextState(context),
            stateOffset = 0,
            statePosState = PosState.initial(name, input),
            stateParseErrors = emptyList()
        )
    }
}

internal infix fun <S : Stream<*, *>, C, E> State<S, C, E>.longestMatch(other: State<S, C, E>): State<S, C, E> =
    when (this.stateOffset compare other.stateOffset) {
        is Ordering.LT -> other
        is Ordering.EQ -> other
        is Ordering.GT -> this
    }
