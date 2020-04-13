package es.weso.rdf.jena

import org.apache.jena.rdf.model.ModelFactory
import java.io.ByteArrayInputStream
import org.apache.jena.rdf.model.Model
import java.io.InputStream
import org.scalatest._
import org.scalatest.matchers.should._


trait JenaBased extends Matchers  {

  def shouldBeIsomorphic(m1: Model, m2: Model): Unit = {
    val b = m1.isIsomorphicWith(m2)
    if (!b) {
      fail(s"""|Models are not isomorphic
               |-------------- Model 1: ${m1.toString}
               |-------------- Model 2: ${m2.toString}""".stripMargin)
    } else ()
  }

  def str2model(s: String): Model = {
    val m = ModelFactory.createDefaultModel
    val in: InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))
    m.read(in, "", "TURTLE")
    m
  } 

}