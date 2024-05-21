interface Stream<S : Any, X : Any> {
    fun uncons(): Pair<X, Stream<S, X>>?

    fun takeN(n: Int): Pair<S, Stream<S, X>>?


    companion object {
        fun of(str: String): Stream<String, Char> = StringStreamImpl(str)
    }
}

typealias StringStream = Stream<String, Char>
typealias ListStream<T> = Stream<List<T>, T>

private data class StringStreamImpl(private val input: String) : StringStream {
    override fun uncons(): Pair<Char, Stream<String, Char>>? =
        input.firstOrNull()?.let { it to StringStreamImpl(input.drop(1)) }

    override fun takeN(n: Int): Pair<String, Stream<String, Char>>? = if (input.isEmpty() && n < 0) {
        null
    } else {
        input.take(n) to StringStreamImpl(input.drop(n))
    }
}


data class ListStreamImpl<T : Any>(private val input: List<T>) : Stream<List<T>, T> {
    override fun uncons(): Pair<T, Stream<List<T>, T>>? =
        input.firstOrNull()?.let { it to ListStreamImpl(input.drop(1)) }

    override fun takeN(n: Int): Pair<List<T>, Stream<List<T>, T>>? = if (input.isEmpty() && n < 0) {
        null
    } else {
        input.take(n) to ListStreamImpl(input.drop(n))
    }

}
