package ar.edu.unq.o3 {

  object Operaciones {

    def ejecutar(micro: Micro, programa: Programa): Unit = {

      def ejecutarInstruccion: (Instruccion) => Unit = instruccion => {
        instruccion match {
          case Add => micro.a = micro.a + micro.b
          case Mul => micro.a = micro.a * micro.b
          case Swap => {
            val temp = micro.a
            micro.a = micro.b
            micro.b = temp
          }
          case Load(posicion) => micro.a = micro.memoria(posicion)
          case Store(posicion) => micro.memoria(posicion) = micro.a
          case If(instrucciones) => if (micro.a == 0) instrucciones.foreach(ejecutarInstruccion)
          case Halt => throw new EjecucionDetenidaException()
        }
      }

      programa.instrucciones.foreach(ejecutarInstruccion)
    }

    def imprimir(programa: Programa): String = {
      def imprimir: (Seq[Instruccion]) => String = instrucciones => {
        instrucciones.map {
          case Add                 => "ADD"
          case Mul                 => "MUL"
          case Swap                => "SWAP"
          case Load(address)       => s"LOAD[$address]"
          case Store(address)      => s"STORE[$address]"
          case If(sub) => s"IF[${imprimir(sub)}]"
          case Halt                => "HALT"
        }.mkString(", ")
      }
      return imprimir(programa.instrucciones)
    }

    def simplificar(programa: Programa): Unit = {
      def simplificar: (List[Instruccion]) => List[Instruccion] = instrucciones => instrucciones match {
        case Nil => Nil
        case Swap :: Swap :: resto => simplificar(resto)
        case Load(_) :: (l: Load) :: resto => simplificar(l :: resto)
        case (s1:Store) :: (s2:Store) :: resto if (s1.posicion == s2.posicion) => simplificar(s2 :: resto)
        case If(sub) :: resto => {
          val simplificadas = simplificar(sub)
          if (simplificadas.isEmpty) simplificar(resto) else If(simplificadas) :: simplificar(resto)
        }
        case Halt :: _ => Halt :: Nil
        case e :: resto => e :: simplificar(resto)
      }
      programa.instrucciones = simplificar(programa.instrucciones)
    }

  }

}