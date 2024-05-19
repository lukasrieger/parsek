import kotlinx.io.files.Path
import kotlin.jvm.JvmInline


data class SourcePos(
    val sourceName: Path,
    val sourceLine: Pos,
    val sourceColumn: Pos
)


@JvmInline
value class Pos(val pos: Int) {
    init {
        require(pos >= 0) { "Position must be > 0" }
    }
}


data class PosState(
    /**
     * The rest of input to process
     */
    val pStateInput: String,
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
)
