package stream

import PosState
import stream.instances.CharListStream
import stream.instances.ListStream
import stream.instances.StringStream


interface Stream<S, X> where S : Any, X : Any {

    fun unpack(): S

    fun uncons(): Pair<X, Stream<S, X>>?

    fun takeN(n: Int): Pair<S, Stream<S, X>>?

    fun takeWhile(test: (X) -> Boolean): Pair<S, Stream<S, X>>

    fun chunkLength(s: S): Int

    fun chunkEmpty(s: S): Boolean

    fun reachOffset(offset: Int, posState: PosState<*>): Pair<String?, PosState<*>> =
        null to reachOffsetNoLine(offset, posState)

    fun reachOffsetNoLine(offset: Int, posState: PosState<*>): PosState<*> =
        reachOffset(offset, posState).second

    companion object {
        fun of(str: String): StringStream = StringStream(str)

        fun of(chars: List<Char>): CharListStream = CharListStream(chars)

        fun <T : Any> of(ls: List<T>): ListStream<T> = ListStream(ls)
    }
}







