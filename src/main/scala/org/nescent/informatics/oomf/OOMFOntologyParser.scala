package org.nescent.informatics.oomf

import java.io.File
import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.AbstractOWLParser
import org.semanticweb.owlapi.io.OWLOntologyDocumentSource
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.AddOntologyAnnotation
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyFormat
import org.semanticweb.owlapi.model.OWLOntologyLoaderConfiguration
import org.semanticweb.owlapi.model.SetOntologyID
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.BufferedReader
import org.apache.commons.io.FileUtils
import org.semanticweb.owlapi.io.StringDocumentSource

class OOMFOntologyParser extends AbstractOWLParser {

	override
	def parse(iri: IRI, ontology: OWLOntology): OWLOntologyFormat = { 
			val mainFile = new File(iri.toURI());
			val shellOntology = loadMainOntology(mainFile);
			this.copyOntologyMetadata(shellOntology, ontology);
			val dataFolder = OOMFOntologyFormat.dataFolderForFile(mainFile);
			this.readFolder(dataFolder, ontology);
			val format = new OOMFOntologyFormat();
			val shellFormat = shellOntology.getOWLOntologyManager().getOntologyFormat(shellOntology);
			format.copyPrefixesFrom(shellFormat.asPrefixOWLOntologyFormat());
			return format;
	}

	override
	def parse(source: OWLOntologyDocumentSource, ontology: OWLOntology): OWLOntologyFormat = {
			return this.parse(source.getDocumentIRI(), ontology);
	}

	override
	def parse(source: OWLOntologyDocumentSource, ontology: OWLOntology, configuration: OWLOntologyLoaderConfiguration): OWLOntologyFormat = {
			// configuration is ignored at the moment
			return this.parse(source, ontology);
	}

	def loadMainOntology(file: File): OWLOntology = {
			val text = FileUtils.readFileToString(file, "UTF-8");
			val ontologyText = text.replaceFirst("%oomf%\\s*", "");
			val manager = OWLManager.createOWLOntologyManager();
			return manager.loadOntologyFromOntologyDocument(new StringDocumentSource(ontologyText));
	}

	def readFolder(folder: File, ontology: OWLOntology): Unit = {
			for (item <- folder.listFiles(DotFileFilter)) {
				if (item.isDirectory()) {
					readFolder(item, ontology);
				} else {
					copyAxioms(item, ontology);
				}
			}
	}

	def copyAxioms(from: File, to: OWLOntology): Unit = {
			val manager = OWLManager.createOWLOntologyManager();
			val ont = manager.loadOntologyFromOntologyDocument(from);
			to.getOWLOntologyManager().addAxioms(to, ont.getAxioms());
	}

	def copyOntologyMetadata(from: OWLOntology, to: OWLOntology): Unit = {
			val toManager = to.getOWLOntologyManager();
			toManager.applyChange(new SetOntologyID(to, from.getOntologyID()));
			val ontologyAxioms = from.getAnnotations().toList.map((new AddOntologyAnnotation(to, _)));
			toManager.applyChanges(ontologyAxioms);
			val ontologyImports = from.getImportsDeclarations().toList.map((new AddImport(to, _)));
			toManager.applyChanges(ontologyImports);
	}

}