package ar.edu.unq.o3 {
  import scala.collection.mutable
  import scala.collection.mutable.ArrayBuffer

  class Micro(
     var a: Int,
     var b: Int,
     var memoria: mutable.Buffer[Int] = 0.to(127).map(_ => 0).to[ArrayBuffer]
   )

  class EjecucionDetenidaException extends Exception

  class Programa(var instrucciones: Seq[Instruccion]) {
    def ejecutar(micro: Micro): Unit = {
      instrucciones.foreach(_.ejecutar(micro))
    }

    def imprimir() = instrucciones.map(_.imprimir).mkString(", ")

    def simplificar() {
      var i = 0
      while (i < instrucciones.size - 1) {
        val instruccion = instrucciones(i)
        val siguiente = instrucciones(i + 1)
        val simplificado = instruccion.simplificar(siguiente)

        instrucciones = instrucciones.take(i) ++ simplificado ++ instrucciones.drop(i + 2);
        if (simplificado == Seq(instruccion, siguiente))
          i += 1
      }
    }

  }

  trait Instruccion {

    def ejecutar(micro: Micro): Unit

    def imprimir(): String

    def simplificar(siguiente: Instruccion): Seq[Instruccion] = Seq(this, siguiente)
  }

  object Add extends Instruccion {
    override def ejecutar(micro: Micro): Unit = micro.a = micro.a + micro.b
    override def imprimir(): String = "ADD"
  }

  object Mul extends Instruccion {
    override def ejecutar(micro: Micro): Unit = micro.a = micro.a * micro.b
    override def imprimir(): String = "MUL"
  }

  object Swap extends Instruccion {
    override def ejecutar(micro: Micro): Unit = {
      val tempB = micro.b
      micro.b = micro.a
      micro.a = tempB
    }
    override def imprimir(): String = "SWAP"

    override def simplificar(siguiente: Instruccion): Seq[Instruccion] = if (siguiente == this) Seq() else super.simplificar(siguiente)
  }

  class Load(val posicion: Int) extends Instruccion {
    override def ejecutar(micro: Micro): Unit = micro.a = micro.memoria(posicion)
    override def imprimir(): String = s"LOAD[${posicion}]"

    override def simplificar(siguiente: Instruccion): Seq[Instruccion] = if (siguiente.isInstanceOf[Load]) Seq(siguiente) else super.simplificar(siguiente)

    override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[Load] && obj.asInstanceOf[Load].posicion == this.posicion
  }

  class Store(val posicion: Int) extends Instruccion {
    override def ejecutar(micro: Micro): Unit = micro.memoria(posicion) = micro.a
    override def imprimir(): String = s"STORE[${posicion}]"

    override def simplificar(siguiente: Instruccion): Seq[Instruccion] = if (siguiente.isInstanceOf[Store]) Seq(siguiente) else super.simplificar(siguiente)
    override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[Store] && obj.asInstanceOf[Store].posicion == this.posicion
  }

  class If(sub: List[Instruccion]) extends Instruccion {
    override def ejecutar(micro: Micro): Unit = if (micro.a == 0) sub.foreach(i => i.ejecutar(micro))
    override def imprimir(): String = s"IF[${sub.map(_.imprimir()).mkString(", ")}]"

    override def simplificar(siguiente: Instruccion): Seq[Instruccion] = if (this.sub.isEmpty) Seq(siguiente) else super.simplificar(siguiente)
  }

  object Halt extends Instruccion {
    override def ejecutar(micro: Micro): Unit = throw new EjecucionDetenidaException
    override def imprimir(): String = "HALT"
  }

}