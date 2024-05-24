@file:Suppress("UNCHECKED_CAST", "FunctionName")

package stream

import Pos
import PosState
import SourcePos
import arrow.core.curried
import arrow.core.identity
import util.Fold
import util.`∘`
import kotlin.math.max


private typealias PairT<Token, Tokens> = Pair<Tokens, Stream<Tokens, Token>>

private typealias SplitState<Token, Tokens> = Pair<String?, PosState<Stream<Tokens, Token>>>


fun <Token : Any, Tokens : Any> reachOffsetH(
    splitAt: (Int, Stream<Tokens, Token>) -> PairT<Token, Tokens>,
    foldl: Fold<Token, Tokens>,
    fromTokens: (Tokens) -> String,
    fromToken: (Token) -> Char,
    newLineTab: Pair<Token, Token>,
    offset: Int,
    posState: PosState<*>
): SplitState<Token, Tokens> {
    val (
        pStateInput,
        pStateOffset,
        pStateSourcePos,
        pStateTabWidth,
        pStateLinePrefix
    ) = posState

    val (
        newLineTok,
        tabTok
    ) = newLineTab

    fun go(st: Pair<SourcePos, (String) -> String>, token: Token): Pair<SourcePos, (String) -> String> {
        val (sourcePos, g) = st
        val (sourceName, sourceLine, sourceColumn) = sourcePos
        val column = sourceColumn.pos
        val tabWidth = pStateTabWidth.pos

        return when (token) {
            newLineTok -> SourcePos(sourceName, sourceLine + Pos.Pos1, Pos.Pos1) to
                    ::identity

            tabTok -> SourcePos(sourceName, sourceLine, Pos(column + tabWidth - ((column - 1) % tabWidth))) to
                    (g `∘` fromToken(token)::plus)

            else -> SourcePos(sourceName, sourceLine, (sourceColumn + Pos.Pos1)) to
                    (g `∘` fromToken(token)::plus)
        }
    }


    val (pre, post) = splitAt(
        offset - pStateOffset,
        pStateInput as Stream<Tokens, Token>
    )
    val (sourcePos, f) = foldl(::go, pStateSourcePos to ::identity, pre)
    val sameLine: Boolean = sourcePos.sourceLine == pStateSourcePos.sourceLine

    fun addPrefix(xs: String) = if (sameLine) pStateLinePrefix + xs else xs
    val expandTabC = ::expandTab.curried()

    val reduce =
        expandTabC(pStateTabWidth) `∘`
                ::addPrefix `∘` f `∘`
                fromTokens `∘` PairT<Token, Tokens>::first

    val reachedLine = reduce(post.takeWhile { it != newLineTok })
        .takeIf { it.isNotEmpty() } ?: "<empty line>"

    return reachedLine to PosState(
        pStateInput = post,
        pStateOffset = max(posState.pStateOffset, offset),
        pStateSourcePos = sourcePos,
        pStateTabWidth = posState.pStateTabWidth,
        pStateLinePrefix = if (sameLine) pStateLinePrefix + f("") else f("")
    )
}

fun <Token, Tokens, S : Stream<Tokens, Token>> reachOffsetNoLineH(
    splitAt: (Int, S) -> Pair<Tokens, S>,
    foldl: Fold<Token, Tokens>,
    newLineTab: Pair<Token, Token>,
    offset: Int,
    posState: PosState<*>
): PosState<S> {
    val (
        pStateInput, pStateOffset,
        pStateSourcePos, pStateTabWidth,
        pStateLinePrefix
    ) = posState

    val (newLineTok, tabTok) = newLineTab

    fun go(sourcePos: SourcePos, token: Token): SourcePos {
        val (sourceName, sourceLine, sourceColumn) = sourcePos
        val column = sourceColumn.pos
        val tabWidth = pStateTabWidth.pos

        return when (token) {
            newLineTok -> SourcePos(sourceName, sourceLine + Pos.Pos1, Pos.Pos1)
            tabTok -> SourcePos(sourceName, sourceLine, Pos(column + tabWidth - ((column - 1) % tabWidth)))
            else -> SourcePos(sourceName, sourceLine, (sourceColumn + Pos.Pos1))
        }
    }

    val (pre, post) = splitAt(offset - pStateOffset, pStateInput as S)
    val sourcePos = foldl(::go, pStateSourcePos, pre)

    return PosState(
        pStateInput = post,
        pStateOffset = max(pStateOffset, offset),
        pStateSourcePos = sourcePos,
        pStateTabWidth = pStateTabWidth,
        pStateLinePrefix = pStateLinePrefix
    )
}

private fun expandTab(pos: Pos, str: String): String {
    fun go(i: Int, o: Int, s: String): String = when {
        o == 0 && s.isEmpty() -> ""
        o == 0 && s.startsWith("\t") -> go(i, (pos.pos - (i % pos.pos)), s.drop(1))
        o == 0 && s.isNotEmpty() -> s.first() + go(i + 1, 0, s.drop(1))
        else -> " " + go(i + 1, o - 1, s)
    }

    return go(0, 0, str)
}