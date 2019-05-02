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


  describe("imprimir") {

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
}
