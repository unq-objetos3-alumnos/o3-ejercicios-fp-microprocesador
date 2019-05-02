package ar.edu.unq.o3 {
  import scala.collection.mutable
  import scala.collection.mutable.ArrayBuffer

  class Micro(
     var a: Int,
     var b: Int,
     var memoria: mutable.Buffer[Int] = 0.to(127).map(_ => 0).to[ArrayBuffer]
   )

  class EjecucionDetenidaException extends Exception

  class Programa(val instrucciones: List[Instruccion]) {
    def ejecutar(micro: Micro): Unit = {
      instrucciones.foreach(_.ejecutar(micro))
    }

    def imprimir() = instrucciones.map(_.imprimir).mkString(", ")
  }

  trait Instruccion {
    def ejecutar(micro: Micro): Unit

    def imprimir(): String
  }

  class Add extends Instruccion {
    override def ejecutar(micro: Micro): Unit = micro.a = micro.a + micro.b
    override def imprimir(): String = "ADD"
  }

  class Mul extends Instruccion {
    override def ejecutar(micro: Micro): Unit = micro.a = micro.a * micro.b
    override def imprimir(): String = "MUL"
  }

  class Swap extends Instruccion {
    override def ejecutar(micro: Micro): Unit = {
      val tempB = micro.b
      micro.b = micro.a
      micro.a = tempB
    }
    override def imprimir(): String = "SWAP"
  }

  class Load(posicion: Int) extends Instruccion {
    override def ejecutar(micro: Micro): Unit = micro.a = micro.memoria(posicion)
    override def imprimir(): String = s"LOAD[${posicion}]"
  }

  class Store(posicion: Int) extends Instruccion {
    override def ejecutar(micro: Micro): Unit = micro.memoria(posicion) = micro.a
    override def imprimir(): String = s"STORE[${posicion}]"
  }

  class If(sub: List[Instruccion]) extends Instruccion {
    override def ejecutar(micro: Micro): Unit = if (micro.a == 0) sub.foreach(i => i.ejecutar(micro))
    override def imprimir(): String = s"IF[${sub.map(_.imprimir()).mkString(", ")}]"
  }

  class Halt extends Instruccion {
    override def ejecutar(micro: Micro): Unit = throw new EjecucionDetenidaException
    override def imprimir(): String = "HALT"
  }

}