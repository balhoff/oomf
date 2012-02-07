package org.nescent.informatics.oomf

import org.semanticweb.owlapi.model.OWLOntologyFormat
import org.semanticweb.owlapi.model.IRI
import java.io.File
import org.semanticweb.owlapi.vocab.PrefixOWLOntologyFormat

object OOMFOntologyFormat {
	val RulesIRI = IRI.create("http://www.nescent.org/ontology/rules");
	val GCIsIRI = IRI.create("http://www.nescent.org/ontology/gcis");
	val AnonymousIRI = IRI.create("http://www.nescent.org/ontology/anonymous");

	def dataFolderForFile(file: File): File = {
			return new File(file.getParent(), file.getName() + ".data");
	}
}

class OOMFOntologyFormat extends PrefixOWLOntologyFormat {}

