import stream.instances.StringStream


val x = pureL<StringStream,_>("Hello World!")
val y = pureL<StringStream, _>(23)

//val a = pureL<StringStream,_>("Hello World!")
//val b: ParsekT2<StringStream, Any, Nothing, Int> = TODO()
//val d: ParsekT2<StringStream, Any, Nothing, Int> = TODO()
//
val c = x