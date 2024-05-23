import kotlin.jvm.JvmInline


typealias FilePath = String

internal fun String.Companion.empty(): FilePath = ""


data class SourcePos(
    val sourceName: FilePath,
    val sourceLine: Pos,
    val sourceColumn: Pos
) {
    companion object {
        fun initial(path: FilePath) = SourcePos(
            sourceName = path,
            sourceLine = Pos.Pos1,
            sourceColumn = Pos.Pos1
        )
    }
}


@JvmInline
value class Pos(val pos: Int) : Comparable<Pos> {
    init {
        require(pos >= 0) { "Position must be > 0" }
    }

    companion object {
        val Pos1 = Pos(1)
        val DefaultTabWidth = Pos(8)
    }

    override fun compareTo(other: Pos): Int = this.pos compareTo other.pos
}


data class PosState<S>(
    /**
     * The rest of input to process
     */
    val pStateInput: S,
    /**
     * Offset corresponding to beginning of [pStateInput]
     */
    val pStateOffset: Int,
    /**
     * Source position corresponding to beginning of [pStateInput]
     */
    val pStateSourcePos: SourcePos,
    /**
     * Tab width to use for column calculation
     */
    val pStateTabWidth: Pos,
    /**
     * Prefix to prepend to offending line
     */
    val pStateLinePrefix: String
) {
    companion object {
        fun <S> initial(name: FilePath, input: S): PosState<S> = PosState(
            pStateInput = input,
            pStateOffset = 0,
            pStateSourcePos = SourcePos.initial(name),
            pStateTabWidth = Pos.DefaultTabWidth,
            pStateLinePrefix = ""
        )
    }
}
