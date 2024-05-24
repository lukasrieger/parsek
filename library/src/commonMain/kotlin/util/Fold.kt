package util

interface Fold<Token, Tokens> {
    operator fun <B> invoke(
        a: (B, Token) -> B,
        b: B,
        tokens: Tokens
    ): B
}