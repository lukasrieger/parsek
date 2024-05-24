package error

import Pos
import util.Ordering

sealed interface ErrorFancy<out E> : Comparable<ErrorFancy<*>> {

    data class ErrorFail(val error: String) : ErrorFancy<Nothing>

    data class ErrorIndentation(val error: Ordering<Pos, Pos>) : ErrorFancy<Nothing>

    data class ErrorCustom<out E>(val error: E) : ErrorFancy<E>

    override fun compareTo(other: ErrorFancy<*>): Int = 0
}

internal fun <E> ErrorFancy<E>.pretty(): String = when (val err = this) {
    is ErrorFancy.ErrorCustom -> err.error.toString()
    is ErrorFancy.ErrorFail -> err.error
    is ErrorFancy.ErrorIndentation -> buildString {
        val (ref, actual) = err.error
        val p = when (err.error) {
            is Ordering.EQ -> "equal to "
            is Ordering.GT -> "greater than "
            is Ordering.LT -> "less than "
        }

        append("incorrect indentation (got")
        append(actual.pos)
        append(", should be")
        append(p)
        append(ref.pos)
        append(")")
    }
}

internal val ErrorFancy<*>.errorFancyLength: Int
    get() = when (this) {
        is ErrorFancy.ErrorCustom -> error.toString().length
        else -> 1
    }