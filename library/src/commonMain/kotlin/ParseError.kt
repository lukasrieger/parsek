import arrow.core.NonEmptyList
import util.Ordering
import util.compare

sealed interface ParseError<out S, out E> {

    val offset: Int

    /**
     * Trivial errors, generated by the parsek.
     */
    data class TrivialError<S, T>(
        /**
         * The error offset
         */
        override val offset: Int,
        /**
         * The unexpected token that was encountered (if any)
         */
        val unexpected: ErrorItem<T>?,
        /**
         * List of expected tokens instead of [unexpected]
         */
        val expected: Set<ErrorItem<T>>
    ) : ParseError<S, Nothing>


    data class FancyError<E>(
        override val offset: Int,
        val errors: Set<ErrorFancy<E>>
    ) : ParseError<Nothing, E>

}


infix fun <S, E> ParseError<S, E>.merge(other: ParseError<S, E>): ParseError<S, E> {
    fun n(a: ErrorItem<*>?, b: ErrorItem<*>?): ErrorItem<*>? = when {
        a == null && b == null -> null
        a != null && b == null -> a
        a == null && b != null -> b
        a != null && b != null -> maxOf(a, b)
        else -> error("unreachable")
    }

    return when (offset compare other.offset) {
        Ordering.EQ -> when {
            this is ParseError.TrivialError<*, *> && other is ParseError.TrivialError<*, *> -> {
                ParseError.TrivialError(
                    this.offset,
                    n(this.unexpected, other.unexpected),
                    this.expected + other.expected
                )
            }

            this is ParseError.FancyError && other is ParseError.TrivialError<*, *> -> this
            this is ParseError.TrivialError<*, *> && other is ParseError.FancyError -> other
            this is ParseError.FancyError && other is ParseError.FancyError ->
                ParseError.FancyError(this.offset, this.errors + other.errors)

            else -> error("unreachable.")
        }

        Ordering.GT -> this
        Ordering.LT -> other
    }
}


internal operator fun <E> ParseError<*, E>.plus(err: ParseError<*, E>): ParseError<*, E> =
    merge(err)


sealed interface ErrorItem<out T> : Comparable<ErrorItem<*>> {

    override fun compareTo(other: ErrorItem<*>): Int = 0


    data class Tokens<T>(
        val tokens: NonEmptyList<T>
    ) : ErrorItem<T>

    data class Label(
        val label: String
    ) : ErrorItem<Nothing>

    data object EndOfInput : ErrorItem<Nothing>
}


sealed interface ErrorFancy<out E> : Comparable<ErrorFancy<*>> {

    override fun compareTo(other: ErrorFancy<*>): Int = 0 // TODO

    data class ErrorFail(val error: String) : ErrorFancy<Nothing>

    data class ErrorIndentation(val error: Ordering) : ErrorFancy<Nothing>

    data class ErrorCustom<E>(val error: E) : ErrorFancy<E>

}

data class ParseErrorBundle<S, E>(
    val bundleErrors: NonEmptyList<ParseError<S, E>>,
    val bundlePosState: PosState<S>
) {

    override fun toString(): String = super.toString()

}