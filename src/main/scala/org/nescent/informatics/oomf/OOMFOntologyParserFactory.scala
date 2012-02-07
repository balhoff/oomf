package org.nescent.informatics.oomf

import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.io.OWLParserFactory
import org.semanticweb.owlapi.io.OWLParser

class OOMFParserFactory extends OWLParserFactory {

	override
	def createParser(manager: OWLOntologyManager): OWLParser = { 
			val parser = new OOMFOntologyParser();
			parser.setOWLOntologyManager(manager);
			return parser;
	}

}