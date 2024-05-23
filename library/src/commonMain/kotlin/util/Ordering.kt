package util


enum class Ordering {
    LT, EQ, GT
}


infix fun <A> A.compare(other: A): Ordering where A : Comparable<A> {
    val ord = this.compareTo(other)
    return when {
        ord == 0 -> Ordering.EQ
        ord > 0 -> Ordering.GT
        ord < 0 -> Ordering.LT
        else -> error("unreachable.")
    }
}