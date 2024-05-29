package stream.instances

import PosState
import arrow.core.identity
import stream.Stream
import stream.reachOffsetH
import stream.reachOffsetNoLineH
import util.Fold
import kotlin.jvm.JvmInline


@JvmInline
value class StringStream(private val input: String) : Stream<String, Char> {

    override fun unpack(): String = input

    override fun toString(): String = input

    override fun uncons(): Pair<Char, Stream<String, Char>>? =
        input.firstOrNull()?.let { it to StringStream(input.drop(1)) }

    override fun takeN(n: Int): Pair<String, Stream<String, Char>>? = if (input.isEmpty() && n < 0) {
        null
    } else {
        input.take(n) to StringStream(input.drop(n))
    }

    override fun chunkToTokens(s: String): List<Char> = s.toList()

    override fun chunkEmpty(s: String): Boolean = s.isEmpty()

    override fun chunkLength(s: String): Int = s.length

    override fun takeWhile(test: (Char) -> Boolean): Pair<String, Stream<String, Char>> =
        input.takeWhile(test).let {
            val y = input.drop(it.length)
            val x = it to StringStream(y)
            x
        }


    override fun reachOffset(
        offset: Int,
        posState: PosState<*>
    ): Pair<String?, PosState<*>> =
        reachOffsetH(
            splitAt = { split, stream ->
                val (str, tail) = stream.unpack().splitAtIndex(split)
                str to StringStream(tail)

            },
            foldl = object : Fold<Char, String> {
                override fun <B> invoke(a: (B, Char) -> B, b: B, tokens: String): B =
                    tokens.fold(b, a)
            },
            fromTokens = ::identity,
            fromToken = ::identity,
            newLineTab = Pair('\n', '\t'),
            offset,
            posState
        )

    override fun reachOffsetNoLine(
        offset: Int,
        posState: PosState<*>
    ): PosState<*> =
        reachOffsetNoLineH(
            splitAt = { split, stream ->
                val (str, tail) = stream.unpack().splitAtIndex(split)

                str to StringStream(tail)

            },
            foldl = object : Fold<Char, String> {
                override fun <B> invoke(a: (B, Char) -> B, b: B, tokens: String): B =
                    tokens.fold(b, a)
            },
            newLineTab = Pair('\n', '\t'),
            offset,
            posState
        )
}

private fun String.splitAtIndex(index: Int) = take(index) to substring(index)
