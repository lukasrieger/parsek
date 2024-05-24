package error

import PosState
import arrow.core.NonEmptyList
import error.ParseError.FancyError
import error.ParseError.TrivialError
import stream.Stream
import kotlin.math.max

data class ParseErrorBundle<S : Stream<*, *>, E>(
    val bundleErrors: NonEmptyList<ParseError<S, E>>,
    val bundlePosState: PosState<S>
) {
    override fun toString(): String = pretty()
}

fun <S : Stream<*, *>> ParseErrorBundle<S, *>.pretty(): String {
    fun f(posState: PosState<out Stream<*, *>>, error: ParseError<*, *>): String {
        val (errorLoc, pst) =
            posState.pStateInput.reachOffset(error.offset, posState)

        val errorPos = pst.pStateSourcePos

        val offendingLine: String = when (errorLoc) {
            null -> ""
            else -> {
                val errorLineLen = errorLoc.length
                val rpShift = errorPos.sourceColumn.pos - 1
                val lineNumber = errorPos.sourceLine.pos.toString()
                val padding = " ".repeat(lineNumber.length + 1)

                val errorLen = when (error) {
                    is FancyError -> error.errors.fold(1) { a, b ->
                        max(a, b.errorFancyLength)
                    }

                    is TrivialError<*, *> -> if (error.unexpected == null) {
                        1
                    } else {
                        error.unexpected.errorItemLength
                    }
                }
                val pointerLen = if (rpShift + errorLen > errorLineLen) {
                    errorLineLen - rpShift + 1
                } else {
                    errorLen
                }
                val rightPadding = if (pointerLen > 0) " ".repeat(rpShift) else ""
                val pointer = "^".repeat(pointerLen)

                buildString {
                    append(padding)
                    append("|\n")
                    append(lineNumber)
                    append(" | ")
                    append(errorLoc)
                    append("\n")
                    append(padding)
                    append("| ")
                    append(rightPadding)
                    append(pointer)
                    append("\n")
                }
            }
        }

        return buildString {
            append("\n")
            append(errorPos.pretty())
            append(":\n")
            append(offendingLine)
            append(error.textPretty())
        }
    }

    return this.bundleErrors.fold("") { acc, b ->
        acc + f(bundlePosState, b)
    }
}