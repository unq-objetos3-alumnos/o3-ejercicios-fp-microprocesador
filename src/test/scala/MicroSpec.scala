import org.scalatest.{FunSpec, Matchers}
import ar.edu.unq.o3._

class MicroSpec extends FunSpec with Matchers {

  describe("Ejecucion") {

    describe("instrucciones") {

      it("ADD suma A y B en A") {
        val micro = new Micro(2, 5)
        val programa = new Programa(List(
          new Add()
        ))
        programa.ejecutar(micro)
        micro.a should equal(2 + 5)
      }

      it("MUL multiplica A y B en A") {
        val micro = new Micro(2, 5)
        val programa = new Programa(List(
          new Mul()
        ))
        programa.ejecutar(micro)
        micro.a should equal(10)
      }

      it("SWAP intercambia A y B") {
        val micro = new Micro(2, 5)
        val programa = new Programa(List(
          new Swap()
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
              new Add(),
              new Add()
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
              new Add(),
              new Add()
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
          new Add(),
          new Halt(),
          // aca frena
          new Add()
        ))
        intercept[EjecucionDetenidaException] {
          programa.ejecutar(micro)
        }
        micro.a should equal(15) // 15, no 20
      }

    }

  }

}
