package es.weso.rdf.operations

object NormalTest {

  trait Builder {
    def add(b: Builder): Builder
    val name: String
  }

  case class JenaBuilder(name: String) extends Builder {
    override def add(x: Builder): Builder = {
      JenaBuilder(name = s"$name + ${x.name}")
    }
  }

}
