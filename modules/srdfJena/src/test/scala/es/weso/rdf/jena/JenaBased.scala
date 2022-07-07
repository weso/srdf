package es.weso.rdf.jena

import org.apache.jena.rdf.model.ModelFactory
import java.io.ByteArrayInputStream
import org.apache.jena.rdf.model.Model
import java.io.InputStream

trait JenaBased {

  def checkIsomorphic(m1: Model, m2: Model): Either[String, Unit] = {
    val b = m1.isIsomorphicWith(m2)
    if (!b) {
      Left(s"""|Models are not isomorphic
               |-------------- Model 1: ${m1.toString}
               |-------------- Model 2: ${m2.toString}""".stripMargin)
    } else Right(())
  }

  /* def shouldBeIsomorphic(m1: Model,m2: Model): Unit = {
    checkIsomorphic(m1,m2).fold(fail(_), _ => ())
  }*/

  def str2model(s: String): Model = {
    val m = ModelFactory.createDefaultModel
    val in: InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))
    m.read(in, "", "TURTLE")
    m
  }

}
