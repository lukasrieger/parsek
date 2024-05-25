import error.ParseError
import stream.Stream
import stream.instances.StringStream


typealias ParsekT2<S, Context, Error, Output> =
            (
            state: State<S, Context, Error>,
            trampoline: (() -> Trampoline<S, Context, Error, Output>) -> Trampoline<S, Context, Error, Output>,
            consumedOk: (Output, State<S, Context, Error>, Hints) -> Trampoline<S, Context, Error, Output>,
            consumedError: (ParseError<Error>, State<S, Context, Error>) -> Trampoline<S, Context, Error, Output>,
            emptyOk: (Output, State<S, Context, Error>, Hints) -> Trampoline<S, Context, Error, Output>,
            emptyError: (ParseError<Error>, State<S, Context, Error>) -> Trampoline<S, Context, Error, Output>
        ) -> Trampoline<S, Context, Error, Output>


typealias Parser2<S, Output> = ParsekT2<S, Any, Nothing, Output>

@Suppress("BOUNDS_NOT_ALLOWED_IF_BOUNDED_BY_TYPE_PARAMETER")
inline operator fun <S : Stream<*, *>, C1, C2, Context, E1, E2, Error, A, B1> ParsekT2<S, C1, E1, A>.times(
    b: ParsekT2<S, C2, E2, B1>
): ParsekT2<S, Context, E2, Pair<A, B1>> where Error : E1, Error : E2, Context : C1, Context : C2 = TODO()


inline fun <S : Stream<*, *>, Output> pureL(
    pure: Output
): Parser2<S,  Output> = { state, trampoline, _, _, emptyOk, _ -> trampoline { emptyOk(pure, state, Hints.empty()) } }


val x = pureL<StringStream,_>("Hello World!")
val y = pureL<StringStream, _>(23)

val a = pureL<StringStream,_>("Hello World!")
val b: ParsekT2<StringStream, Any, Nothing, Int> = TODO()
val d: ParsekT2<StringStream, Any, Nothing, Int> = TODO()

val c = b * d

