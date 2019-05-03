import org.scalatest.{FunSpec, Matchers}
import ar.edu.unq.o3._
import ar.edu.unq.o3.Operaciones._

class MicroSpec extends FunSpec with Matchers {

  describe("ejecutar") {

    describe("instrucciones") {

      it("ADD suma A y B en A") {
        val micro = new Micro(2, 5)
        val programa = new Programa(List(
          Add
        ))
        ejecutar(micro, programa).a should equal(2 + 5)
      }

      it("MUL multiplica A y B en A") {
        val micro = new Micro(2, 5)
        val programa = new Programa(List(
          Mul
        ))
        ejecutar(micro, programa).a should equal(10)
      }

      it("SWAP intercambia A y B") {
        val micro = new Micro(2, 5)
        val programa = new Programa(List(
          Swap
        ))
        val newMicro = ejecutar(micro, programa)
        newMicro.a should equal(5)
        newMicro.b should equal(2)
      }

      it("LOAD(pos) carga el contenido de la posicion en A") {
        val micro = new Micro(2, 5, 0.to(127).map(_ => 0).toList.updated(6, 42))
        val programa = new Programa(List(
          Load(6)
        ))
        ejecutar(micro, programa).a should equal(42)
      }

      it("STORE(pos) guarda el contenido de A en pos") {
        val micro = new Micro(42, 5)
        val programa = new Programa(List(
          Store(6)
        ))
        ejecutar(micro, programa).memoria(6) should equal(42)
      }

      describe("IF(sub)") {

        it("no ejecuta las subinstrucciones si A es no es 0") {
          val micro = new Micro(42, 5)
          val programa = new Programa(List(
            If(List(
              Add,
              Add
            ))
          ))
          val newMicro = ejecutar(micro, programa)
          newMicro.a should equal(42)
          newMicro.b should equal(5)
        }

        it("ejecuta las subinstrucciones si A es es 0") {
          val micro = new Micro(0, 5)
          val programa = new Programa(List(
            If(List(
              Add,
              Add
            ))
          ))
          val newMicro = ejecutar(micro, programa)
          newMicro.a should equal(10)
          newMicro.b should equal(5)
        }

      }

      it("HALT interrumpe con una exception y no ejecuta lo que sigue") {
        val micro = new Micro(10, 5)
        val programa = new Programa(List(
          Add,
          Halt,
          // aca frena
          Add
        ))
        val exception = intercept[EjecucionDetenidaException] {
          ejecutar(micro, programa)
        }
        exception.micro.a should equal(15) // 15, no 20
      }

    }

  }


  describe("imprimir()") {

    it("imprime un conjunto de instrucciones separadas por coma y con parámetros entre corchetes") {
      val p = new Programa(List(
        Add,
        Mul,
        Swap,
        Load(23),
        Store(42),
        If(List(Add, Add)),
        Halt
      ))

      imprimir(p) should equal("ADD, MUL, SWAP, LOAD[23], STORE[42], IF[ADD, ADD], HALT")
    }

  }

  describe("simplificar()") {

    it("remueve 2 SWAP consecutivos, no swaps intercalados") {
      val p = new Programa(List(
        Swap,
        Swap,
        Add,
        Swap,
        Add,
        Swap
      ))
      simplificar(p)
      p.instrucciones should equal(List(
        Add,
        Swap,
        Add,
        Swap
      ))
    }

    it("se banca hacer 2 simplificaciones en el programa") {
      val p = new Programa(List(
        Swap,
        Swap,
        Add,
        Swap,
        Swap
      ))
      simplificar(p)
      p.instrucciones should equal(List(
        Add,
      ))
    }

    it("elimina el primer LOAD si hay 2 consecutivos") {
      val p = new Programa(List(
        Load(23),
        Load(42)
      ))
      simplificar(p)
      p.instrucciones should equal(List(
        Load(42),
      ))
    }

    it("se banca simplificar el resultado de haber simplificado un LOAD que causa otro LOAD + LOAD") {
      val p = new Programa(List(
        Load(1),
        Load(2),
        Load(3)
      ))
      simplificar(p)
      p.instrucciones should equal(List(
        Load(3),
      ))
    }

    it("elimina el primer STORE si hay 2 consecutivos con la misma direccion") {
      val p = new Programa(List(
        Store(23),
        Store(23)
      ))
      simplificar(p)
      p.instrucciones should equal(List(
        Store(23)
      ))
    }

    it("NO elimina el primer STORE si son consecutivos pero de diferente direccion") {
      val p = new Programa(List(
        Store(23),
        Store(42)
      ))
      simplificar(p)
      p.instrucciones should equal(List(
        Store(23),
        Store(42)
      ))
    }

    it("elimina un IF que no tiene subinstrucciones") {
      val p = new Programa(List(
        If(List()),
        Store(23)
      ))
      simplificar(p)
      p.instrucciones should equal(List(
        Store(23),
      ))
    }

    it("elimina un IF que no tiene subinstrucciones que está al final") {
      val p = new Programa(List(
        Store(23),
        If(List())
      ))
      simplificar(p)
      p.instrucciones should equal(List(
        Store(23),
      ))
    }

  }

}
