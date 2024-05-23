import kotlin.jvm.JvmInline

interface Stream<S, X> where S : Any, X : Any {
    fun uncons(): Pair<X, Stream<S, X>>?

    fun takeN(n: Int): Pair<S, Stream<S, X>>?

    fun takeWhile(test: (X) -> Boolean): Pair<S, Stream<S, X>>

    fun chunkLength(s: S): Int

    fun chunkEmpty(s: S): Boolean

    companion object {
        fun of(str: String): Stream<String, Char> = StringStreamImpl(str)
    }
}

typealias StringStream = Stream<String, Char>
typealias ListStream<T> = Stream<List<T>, T>

@JvmInline
private value class StringStreamImpl(private val input: String) : StringStream {

    override fun toString(): String = input

    override fun uncons(): Pair<Char, Stream<String, Char>>? =
        input.firstOrNull()?.let { it to StringStreamImpl(input.drop(1)) }

    override fun takeN(n: Int): Pair<String, Stream<String, Char>>? = if (input.isEmpty() && n < 0) {
        null
    } else {
        input.take(n) to StringStreamImpl(input.drop(n))
    }

    override fun chunkEmpty(s: String): Boolean {
        TODO("Not yet implemented")
    }

    override fun chunkLength(s: String): Int {
        TODO("Not yet implemented")
    }

    override fun takeWhile(test: (Char) -> Boolean): Pair<String, Stream<String, Char>> {
        TODO("Not yet implemented")
    }
}

@JvmInline
value class ListStreamImpl<T : Any>(private val input: List<T>) : Stream<List<T>, T> {
    override fun uncons(): Pair<T, Stream<List<T>, T>>? =
        input.firstOrNull()?.let { it to ListStreamImpl(input.drop(1)) }

    override fun takeN(n: Int): Pair<List<T>, Stream<List<T>, T>>? = if (input.isEmpty() && n < 0) {
        null
    } else {
        input.take(n) to ListStreamImpl(input.drop(n))
    }

    override fun chunkEmpty(s: List<T>): Boolean {
        TODO("Not yet implemented")
    }

    override fun chunkLength(s: List<T>): Int {
        TODO("Not yet implemented")
    }

    override fun takeWhile(test: (T) -> Boolean): Pair<List<T>, Stream<List<T>, T>> {
        TODO("Not yet implemented")
    }
}