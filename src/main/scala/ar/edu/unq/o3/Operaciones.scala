package ar.edu.unq.o3 {

  object Operaciones {

    implicit def programaAInstrucciones(programa: Programa): List[Instruccion] = programa.instrucciones

    //
    // Ejecutar
    //

    def ejecutarInstruccion = (micro: Micro, instruccion: Instruccion) => {
      println(s" > Ejecutando ${imprimir(instruccion)}")
      instruccion match {
        case Add => micro.copy(a = micro.a + micro.b)
        case Mul => micro.copy(a = micro.a * micro.b)
        case Swap => micro.copy(a = micro.b, b = micro.a)
        case Load(posicion) => micro.copy(a = micro.memoria(posicion))
        case Store(posicion) => micro.copy(memoria = micro.memoria.updated(posicion, micro.a))
        case If(instrucciones) => if (micro.a == 0) ejecutar(micro, instrucciones) else micro
        case Halt => throw new EjecucionDetenidaException(micro)
      }
    }

    def ejecutar(microInicial: Micro, instrucciones: List[Instruccion]): Micro = {
      instrucciones.foldLeft(microInicial)(ejecutarInstruccion)
    }

    def memoizar(function: (Micro, Instruccion) => Micro): (Micro, Instruccion) => Micro = {
      var cache = Map[(Micro, Instruccion), Micro]()

      def ejecutarYCachear(micro: Micro, instruccion: Instruccion)(): Micro = {
        val nuevoMicro: Micro = function(micro, instruccion)
        cache += ((micro, instruccion) -> nuevoMicro)
        nuevoMicro
      }

      return (micro, instruccion) => cache.get(micro, instruccion).getOrElse(ejecutarYCachear(micro, instruccion))
    }

    def ejecutarMemoizando(microInicial: Micro, instrucciones: List[Instruccion]): Micro = {
      instrucciones.foldLeft(microInicial)(memoizar(ejecutarInstruccion))
    }

    //
    // Imprimir
    //

    def imprimir(instruccion: Instruccion): String = instruccion match {
      case Add                 => "ADD"
      case Mul                 => "MUL"
      case Swap                => "SWAP"
      case Load(address)       => s"LOAD[$address]"
      case Store(address)      => s"STORE[$address]"
      case If(sub) => s"IF[${imprimir(sub)}]"
      case Halt                => "HALT"
    }

    def imprimir(instrucciones: Seq[Instruccion]): String = instrucciones.map(imprimir).mkString(", ")

    //
    // Simplificar
    //

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