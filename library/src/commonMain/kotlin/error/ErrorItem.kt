package error

import arrow.core.NonEmptyList

sealed interface ErrorItem<out T> : Comparable<ErrorItem<*>> {

    override fun compareTo(other: ErrorItem<*>): Int = 0

    data class Tokens<out T>(
        val tokens: NonEmptyList<T>
    ) : ErrorItem<T>

    data class Label(
        val label: String
    ) : ErrorItem<Nothing>

    data object EndOfInput : ErrorItem<Nothing>
}

internal val ErrorItem<*>.errorItemLength: Int
    get() = when (this) {
        is ErrorItem.Tokens -> this.tokens.map { it.toString().length }.sum()
        else -> 1
    }

internal fun ErrorItem<*>.pretty(): String = when (this) {
    is ErrorItem.EndOfInput -> "end of input"
    is ErrorItem.Label -> label
    is ErrorItem.Tokens -> tokens.joinToString("") { it.prettyChar() ?: it.toString() }
}

private fun Any?.prettyChar() = when (this) {
    ' ' -> "space"
    '\t' -> "tab"
    '\n' -> "newline"
    '\r' -> "carriage return"
    else -> null
}?.let { "<$it>" }


