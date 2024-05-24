package stream.instances

import stream.Stream

interface ListStream<T : Any> : Stream<List<T>, T> {

    override fun unpack(): List<T>

    override fun uncons(): Pair<T, Stream<List<T>, T>>? =
        unpack().firstOrNull()?.let { it to ListStream(unpack().drop(1)) }

    override fun takeN(n: Int): Pair<List<T>, Stream<List<T>, T>>? = if (unpack().isEmpty() && n < 0) {
        null
    } else {
        unpack().take(n) to ListStream(unpack().drop(n))
    }

    override fun chunkEmpty(s: List<T>): Boolean = s.isEmpty()

    override fun chunkLength(s: List<T>): Int = s.size

    override fun takeWhile(test: (T) -> Boolean): Pair<List<T>, Stream<List<T>, T>> =
        unpack().takeWhile(test).let { it to ListStream(unpack().drop(it.size)) }

    companion object {
        operator fun <T : Any> invoke(ls: List<T>): ListStream<T> = object : ListStream<T> {
            override fun unpack(): List<T> = ls
        }
    }
}