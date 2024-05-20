
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
) {
    companion object {
        fun <Error, Context> initial(
            name: FilePath,
            input: String,
            context: Context
        ): State<Error, Context> = State(
            stateInput = input,
            stateContext = ContextState(context),
            stateOffset = 0,
            statePosState = PosState.initial(name, input),
            stateParseErrors = emptyList()
        )
    }
}