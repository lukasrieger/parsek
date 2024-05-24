package util

import arrow.core.compose

internal infix fun <I, R, O> ((I) -> R).`∘`(f: (O) -> I): (O) -> R =
    this compose f
