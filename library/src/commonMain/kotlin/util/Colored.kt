package util


private const val ESCAPE = '\u001B'
private const val RESET = "$ESCAPE[0m"

internal fun color(string: String, color: Color) = color(string, color.foreground)

private fun color(string: String, ansiString: String) = "$ansiString$string$RESET"

fun String.black() = color(this, Color.BLACK)

fun String.red() = color(this, Color.RED)

fun String.green() = color(this, Color.GREEN)

fun String.yellow() = color(this, Color.YELLOW)

fun String.blue() = color(this, Color.BLUE)

fun String.magenta() = color(this, Color.MAGENTA)

fun String.cyan() = color(this, Color.CYAN)

fun String.lightGray() = color(this, Color.LIGHT_GRAY)

fun String.lightRed() = color(this, Color.LIGHT_RED)

fun String.lightGreen() = color(this, Color.LIGHT_GREEN)

fun String.lightYellow() = color(this, Color.LIGHT_YELLOW)

fun String.lightBlue() = color(this, Color.LIGHT_BLUE)

fun String.lightMagenta() = color(this, Color.LIGHT_MAGENTA)

fun String.lightCyan() = color(this, Color.LIGHT_CYAN)

fun String.lightWhite() = color(this, Color.WHITE)



private const val BG_JUMP = 10

internal enum class Color(baseCode: Int) {
    BLACK(30),
    RED(31),
    GREEN(32),
    YELLOW(33),
    BLUE(34),
    MAGENTA(35),
    CYAN(36),
    LIGHT_GRAY(37),

    DARK_GRAY(90),
    LIGHT_RED(91),
    LIGHT_GREEN(92),
    LIGHT_YELLOW(93),
    LIGHT_BLUE(94),
    LIGHT_MAGENTA(95),
    LIGHT_CYAN(96),
    WHITE(97);


    val foreground: String = "$ESCAPE[${baseCode}m"

    val background: String = "$ESCAPE[${baseCode + BG_JUMP}m"
}