package util

sealed interface Ordering<A, B> {
    val a: A
    val b: B

    operator fun component1(): A = a
    operator fun component2(): B = b

    data class LT<A, B>(override val a: A, override val b: B) : Ordering<A, B>

    data class EQ<A, B>(override val a: A, override val b: B) : Ordering<A, B>

    data class GT<A, B>(override val a: A, override val b: B) : Ordering<A, B>

}


infix fun <A> A.compare(other: A): Ordering<A, A> where A : Comparable<A> {
    val ord = this.compareTo(other)
    return when {
        ord == 0 -> Ordering.EQ(this, other)
        ord > 0 -> Ordering.GT(this, other)
        ord < 0 -> Ordering.LT(this, other)
        else -> error("unreachable.")
    }
}