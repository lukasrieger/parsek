package stream.instances

import stream.Stream
import kotlin.jvm.JvmInline


@JvmInline
value class CharListStream(private val input: List<Char>) : Stream<List<Char>, Char> {

    override fun unpack(): List<Char> = input

    override fun uncons(): Pair<Char, Stream<List<Char>, Char>>? =
        unpack().firstOrNull()?.let { it to of(unpack().drop(1)) }

    override fun takeN(n: Int): Pair<List<Char>, Stream<List<Char>, Char>>? = if (unpack().isEmpty() && n < 0) {
        null
    } else {
        unpack().take(n) to of(unpack().drop(n))
    }

    override fun chunkToTokens(s: List<Char>): List<Char> = s

    override fun chunkEmpty(s: List<Char>): Boolean = s.isEmpty()

    override fun chunkLength(s: List<Char>): Int = s.size

    override fun takeWhile(test: (Char) -> Boolean): Pair<List<Char>, Stream<List<Char>, Char>> =
        unpack().takeWhile(test).let { it to of(unpack().drop(it.size)) }

    companion object {
        fun <T : Any> of(ls: List<T>): ListStream<T> = object : ListStream<T> {
            override fun unpack(): List<T> = ls
            override fun chunkToTokens(s: List<T>): List<T> = s
        }
    }
}