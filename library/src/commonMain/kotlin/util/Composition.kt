package util

import arrow.core.compose

internal infix fun <I, R, O> ((I) -> R).`∘`(f: (O) -> I): (O) -> R =
    this compose f

internal fun <P1, P2, R> ((P1, P2) -> R)._1(p1: P1): (P2) -> R =
    { p2: P2 -> this(p1, p2) }