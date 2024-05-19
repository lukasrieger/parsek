package util

sealed interface Ordering<A, B> {

    data class LT<A, B>(val a: A, val b: B) : Ordering<A, B>

    data class EQ<A, B>(val a: A, val b: B) : Ordering<A, B>

    data class GT<A, B>(val a: A, val b: B) : Ordering<A, B>

}