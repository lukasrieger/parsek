package util

import arrow.core.NonEmptyList
import arrow.core.toNonEmptyListOrNull

fun String.toNonEmptyList(): NonEmptyList<Char> =
    toList().toNonEmptyListOrNull() ?: error("Given string was empty, expected at least one character.")