package ar.edu.unq.o3 {
  import scala.collection.mutable
  import scala.collection.mutable.ArrayBuffer

  class Micro(
     var a: Int,
     var b: Int,
     var memoria: mutable.Buffer[Int] = 0.to(127).map(_ => 0).to[ArrayBuffer]
   )

  class EjecucionDetenidaException extends Exception

  class Programa(var instrucciones: List[Instruccion])

  trait Instruccion {}

  object Add extends Instruccion
  object Mul extends Instruccion
  object Swap extends Instruccion
  case class Load(val posicion: Int) extends Instruccion
  case class Store(val posicion: Int) extends Instruccion
  case class If(sub: List[Instruccion]) extends Instruccion
  object Halt extends Instruccion

}