package util

import arrow.core.NonEmptyList
import arrow.core.toNonEmptyListOrNull

fun String.toNonEmptyList(): NonEmptyList<Char> =
    toList().toNonEmptyListOrNull() ?: error("Given string was empty, expected at least one character.")


fun String.take1(): Pair<Char, String>? =
    firstOrNull()?.let { it to this.drop(1) }

fun String.takeN(n: Int): Pair<String, String>? =
    if (isEmpty() && n < 0) {
        null
    } else {
        take(n) to drop(n)
    }