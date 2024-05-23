@file:Suppress("UNCHECKED_CAST")

import arrow.core.compose
import arrow.core.identity
import arrow.core.partially1
import kotlin.jvm.JvmInline
import kotlin.math.max

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
        fun of(str: String): Stream<String, Char> = StringStreamImpl(str)
    }
}

typealias StringStream = Stream<String, Char>
typealias ListStream<T> = Stream<List<T>, T>

@JvmInline
private value class StringStreamImpl(val input: String) : StringStream {

    override fun unpack(): String = input

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

    override fun takeWhile(test: (Char) -> Boolean): Pair<String, Stream<String, Char>> =
        input.takeWhile(test).let { it to StringStreamImpl(input.drop(it.length)) }


    override fun reachOffset(
        offset: Int,
        posState: PosState<*>
    ): Pair<String?, PosState<*>> =
        reachOffsetH(
            splitAt = { split, stream ->
                val (str, tail) = stream.unpack().splitAtIndex(split)
                str to StringStreamImpl(tail)

            },
            foldl = object : FoldL<Char, String> {
                override fun <B> invoke(a: (B, Char) -> B, b: B, tokens: String): B =
                    tokens.fold(b, a)
            },
            fromToks = ::identity,
            fromTok = ::identity,
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

                str to StringStreamImpl(tail)

            },
            foldl = object : FoldL<Char, String> {
                override fun <B> invoke(a: (B, Char) -> B, b: B, tokens: String): B =
                    tokens.fold(b, a)
            },
            newLineTab = Pair('\n', '\t'),
            offset,
            posState
        )
}


private fun String.splitAtIndex(index: Int) = take(index) to substring(index)


interface FoldL<Token, Tokens> {
    operator fun <B> invoke(
        a: (B, Token) -> B,
        b: B,
        tokens: Tokens
    ): B
}


fun <Token : Any, Tokens : Any> reachOffsetH(
    splitAt: (Int, Stream<Tokens, Token>) -> Pair<Tokens, Stream<Tokens, Token>>,
    foldl: FoldL<Token, Tokens>,
    fromToks: (Tokens) -> String,
    fromTok: (Token) -> Char,
    newLineTab: Pair<Token, Token>,
    offset: Int,
    o: PosState<*>
): Pair<String?, PosState<Stream<Tokens, Token>>> {
    val (newLineTok, tabTok) = newLineTab
    fun go(st: Pair<SourcePos, (String) -> String>, token: Token): Pair<SourcePos, (String) -> String> {
        val (sourcePos, g) = st
        val (sn, sl, sc) = sourcePos
        val c = sc.pos
        val w = o.pStateTabWidth.pos

        return when (token) {
            newLineTok -> SourcePos(sn, sl + Pos.Pos1, Pos.Pos1) to ::identity
            tabTok -> SourcePos(sn, sl, Pos(c + w - ((c - 1) % w))) to
                    g.compose(String::plus.partially1(fromTok(token).toString()))

            else -> SourcePos(sn, sl, (sc + Pos.Pos1)) to
                    g.compose(String::plus.partially1(fromTok(token).toString()))
        }
    }


    val (pre, post) =
        splitAt(offset - o.pStateOffset, o.pStateInput as Stream<Tokens, Token>)
    val (spos, f) = foldl(::go, o.pStateSourcePos to ::identity, pre)
    val sameLine: Boolean = spos.sourceLine == o.pStateSourcePos.sourceLine


    fun addPrefix(xs: String) = if (sameLine) {
        o.pStateLinePrefix + xs
    } else {
        xs
    }


    val transformer =
        ::expandTab.partially1(o.pStateTabWidth) compose
                ::addPrefix compose
                f compose
                fromToks compose
                Pair<Tokens, Stream<Tokens, Token>>::first

    val x = when (val xs = transformer(post.takeWhile { it != newLineTok })) {
        "" -> "<empty line>"
        else -> xs
    }

    return x to PosState(
        pStateInput = post,
        pStateOffset = max(o.pStateOffset, offset),
        pStateSourcePos = spos,
        pStateTabWidth = o.pStateTabWidth,
        pStateLinePrefix = if (sameLine) {
            o.pStateLinePrefix + f("")
        } else {
            f("")
        }
    )
}

fun <Token, Tokens, S : Stream<Tokens, Token>> reachOffsetNoLineH(
    splitAt: (Int, S) -> Pair<Tokens, S>,
    foldl: FoldL<Token, Tokens>,
    newLineTab: Pair<Token, Token>,
    offset: Int,
    o: PosState<*>
): PosState<S> {
    val (newLineTok, tabTok) = newLineTab

    fun go(sourcePos: SourcePos, token: Token): SourcePos {
        val (sn, sl, sc) = sourcePos
        val c = sc.pos
        val w = o.pStateTabWidth.pos

        return when (token) {
            newLineTok -> SourcePos(sn, sl + Pos.Pos1, Pos.Pos1)
            tabTok -> SourcePos(sn, sl, Pos(c + w - ((c - 1) % w)))
            else -> SourcePos(sn, sl, (sc + Pos.Pos1))
        }
    }

    val (pre, post) = splitAt(offset - o.pStateOffset, o.pStateInput as S)
    val spos = foldl(::go, o.pStateSourcePos, pre)

    return PosState(
        pStateInput = post,
        pStateOffset = max(o.pStateOffset, offset),
        pStateSourcePos = spos,
        pStateTabWidth = o.pStateTabWidth,
        pStateLinePrefix = o.pStateLinePrefix
    )
}


fun expandTab(pos: Pos, str: String): String {
    fun go(i: Int, o: Int, s: String): String = when {
        o == 0 && s.isEmpty() -> ""
        o == 0 && s.startsWith("\t") -> go(i, (pos.pos - (i % pos.pos)), s.drop(1))
        o == 0 && s.isNotEmpty() -> s.first() + go(i + 1, 0, s.drop(1))
        else -> " " + go(i + 1, o - 1, s)
    }

    return go(0, 0, str)
}


@JvmInline
value class ListStreamImpl<T : Any>(private val input: List<T>) : Stream<List<T>, T> {

    override fun unpack(): List<T> = input

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