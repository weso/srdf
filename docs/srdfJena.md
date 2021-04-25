---
id: srdfJena
title: SRDFJena
---

# SRDFJena 

SRDFJena contains an implementation of SRDF based on Apache Jena models.

The `RDFAsJenaModel` class implements the different interfaces.

## Example usage

```scala mdoc
import es.weso.rdf.jena._
import es.weso.rdf.nodes._
import es.weso.rdf.triples._

val ex = IRI("http://example.org/")
val schema = IRI("http://schema.org/")

val alice = ex + "alice"
val knows = schema + "bob"
val bob   = ex + "bob" 
val t1 = RDFTriple(alice,knows,bob)

val showTriples = for {
  emptyResource <- RDFAsJenaModel.empty
  _ <- emptyResource.use(rdf => for {
     newRdf <- rdf.addTriple(t1)
     _ <- newRdf.serialize("TURTLE")
    } yield ()
  )
} yield ()

import cats.effect.unsafe.implicits.global

showTriples.unsafeRunSync()
```