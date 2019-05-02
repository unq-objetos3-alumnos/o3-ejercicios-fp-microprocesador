import org.scalatest.{FunSpec, Matchers}
import ar.edu.unq.o3._

class MicroSpec extends FunSpec with Matchers {

  describe("ejecutar") {

    describe("instrucciones") {

      it("ADD suma A y B en A") {
        val micro = new Micro(2, 5)
        val programa = new Programa(List(
          Add
        ))
        programa.ejecutar(micro)
        micro.a should equal(2 + 5)
      }

      it("MUL multiplica A y B en A") {
        val micro = new Micro(2, 5)
        val programa = new Programa(List(
          Mul
        ))
        programa.ejecutar(micro)
        micro.a should equal(10)
      }

      it("SWAP intercambia A y B") {
        val micro = new Micro(2, 5)
        val programa = new Programa(List(
          Swap
        ))
        programa.ejecutar(micro)
        micro.a should equal(5)
        micro.b should equal(2)
      }

      it("LOAD(pos) carga el contenido de la posicion en A") {
        val micro = new Micro(2, 5)
        micro.memoria(6) = 42
        val programa = new Programa(List(
          new Load(6)
        ))
        programa.ejecutar(micro)
        micro.a should equal(42)
      }

      it("STORE(pos) guarda el contenido de A en pos") {
        val micro = new Micro(42, 5)
        val programa = new Programa(List(
          new Store(6)
        ))
        programa.ejecutar(micro)
        micro.memoria(6) should equal(42)
      }

      describe("IF(sub)") {

        it("no ejecuta las subinstrucciones si A es no es 0") {
          val micro = new Micro(42, 5)
          val programa = new Programa(List(
            new If(List(
              Add,
              Add
            ))
          ))
          programa.ejecutar(micro)
          micro.a should equal(42)
          micro.b should equal(5)
        }

        it("ejecuta las subinstrucciones si A es es 0") {
          val micro = new Micro(0, 5)
          val programa = new Programa(List(
            new If(List(
              Add,
              Add
            ))
          ))
          programa.ejecutar(micro)
          micro.a should equal(10)
          micro.b should equal(5)
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
        intercept[EjecucionDetenidaException] {
          programa.ejecutar(micro)
        }
        micro.a should equal(15) // 15, no 20
      }

    }

  }


  describe("imprimir()") {

    it("imprime un conjunto de instrucciones separadas por coma y con parámetros entre corchetes") {
      val p = new Programa(List(
        Add,
        Mul,
        Swap,
        new Load(23),
        new Store(42),
        new If(List(Add, Add)),
        Halt
      ))

      p.imprimir() should equal("ADD, MUL, SWAP, LOAD[23], STORE[42], IF[ADD, ADD], HALT")
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
      p.simplificar()
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
      p.simplificar()
      p.instrucciones should equal(List(
        Add,
      ))
    }

    it("elimina el primer LOAD si hay 2 consecutivos") {
      val p = new Programa(List(
        new Load(23),
        new Load(42)
      ))
      p.simplificar()
      p.instrucciones should equal(List(
        new Load(42),
      ))
    }

    it("se banca simplificar el resultado de haber simplificado un LOAD que causa otro LOAD + LOAD") {
      val p = new Programa(List(
        new Load(1),
        new Load(2),
        new Load(3)
      ))
      p.simplificar()
      p.instrucciones should equal(List(
        new Load(3),
      ))
    }

    it("elimina el primer STORE si hay 2 consecutivos") {
      val p = new Programa(List(
        new Store(23),
        new Store(42)
      ))
      p.simplificar()
      p.instrucciones should equal(List(
        new Store(42),
      ))
    }

    it("elimina un IF que no tiene subinstrucciones") {
      val p = new Programa(List(
        new If(List()),
        new Store(23)
      ))
      p.simplificar()
      p.instrucciones should equal(List(
        new Store(23),
      ))
    }

    it("elimina un IF que no tiene subinstrucciones que está al final") {
      val p = new Programa(List(
        new Store(23),
        new If(List())
      ))
      p.simplificar()
      p.instrucciones should equal(List(
        new Store(23),
      ))
    }

  }

}
