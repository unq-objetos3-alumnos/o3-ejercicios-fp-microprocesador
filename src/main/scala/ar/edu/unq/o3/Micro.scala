package ar.edu.unq.o3 {

  case class Micro(
    val a: Int,
    val b: Int,
    val memoria: List[Int] = 0.to(127).map(_ => 0).toList
  )

  case class EjecucionDetenidaException(val micro: Micro) extends Exception

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