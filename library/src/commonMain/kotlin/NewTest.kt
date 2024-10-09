

typealias Pars<R, E, O> = (R) -> Repl<in R, E, O>


inline infix fun <R, E, O> Pars<R, E, O>.or(
    other: Pars<R, E, O>
): Pars<R, E, O> = TODO()

val p_1: Pars<Any, Nothing, Int> = TODO()
val p_2: Pars<String, Throwable, Nothing> = TODO()

val testtt = p_1 or p_2