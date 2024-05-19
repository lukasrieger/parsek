import State
import fp.serrano.inikio.ProgramBuilder
import fp.serrano.inikio.program
import kotlinx.io.files.Path
import kotlin.experimental.ExperimentalTypeInference
import kotlin.reflect.KProperty0
import State as StateP


class ParsekScope<Error, Context, O> : ProgramBuilder<ParsekT<Error, Context, O>, O>(::pure) {
    suspend fun context(): Context = perform {
        object : ParsekT<Error, Context, Context> {
            context(Context) override fun <B> unparser(
                state: StateP<Error>,
                consumedOk: ConsumedOk<Error, Context, Context, B>,
                consumedError: ConsumedError<Error, Context, B>,
                emptyOk: EmptyOk<Error, Context, Context, B>,
                emptyError: EmptyError<Error, Context, B>
            ): B = emptyOk(this@Context, this@Context, state, Hints.empty())
        }.bind(it)
    }

    suspend operator fun <R> ParsekT<Error, Context, R>.not(): R = perform { cont ->
        bind(cont)
    }
}

@OptIn(ExperimentalTypeInference::class)
@BuilderInference
fun <E, C, O> doM(@BuilderInference f: suspend ParsekScope<E, C, O>.() -> O): ParsekT<E, C, O> {

//    val fn: suspend ParsekScope<E, C, O>.() -> O = {
//            f(this, context())
//
//    }
    return program(ParsekScope(), f)
}

class St(var callCount: Int)
val t1: ParsekT<Nothing, St, String> = pure("Hello!")

val t2: ParsekT<Nothing, Nothing, Int> = pure(0)


val t3: ParsekT<Nothing, St, String> get() = doM {
    val first = !t1
    val second = !t2

    "$first -> $second"

    val cC = context()

    cC.callCount += 1


    println(cC.callCount)

    !ref(::t3)
}

fun <E, C, O> ref(
    parserProperty: KProperty0<ParsekT<E, C, O>>
) : ParsekT<E, C, O> {
    return parserProperty()
}


fun main() {
    println(t3)


    St(0).run {
        println(t3.runParsekT(
            State(
                stateInput = "",
                stateOffset = 0,
                statePosState = PosState(
                    pStateLinePrefix = "",
                    pStateOffset = 0,
                    pStateInput = "",
                    pStateTabWidth = Pos(4),
                    pStateSourcePos = SourcePos(
                        sourceColumn = Pos(0),
                        sourceLine = Pos(0),
                        sourceName = Path("")
                    )
                ),
                stateParseErrors = emptyList()
            )
        ).result
        )
    }

}