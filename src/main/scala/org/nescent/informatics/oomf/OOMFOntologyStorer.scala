package org.nescent.informatics.oomf

import java.io.File
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.apache.commons.io.FileUtils
import org.semanticweb.owlapi.io.OWLOntologyDocumentTarget
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyFormat
import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.model.OWLOntologyStorer
import org.semanticweb.owlapi.util.OWLOntologyWalker
import java.net.URLEncoder
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.OWLFunctionalSyntaxOntologyFormat
import org.apache.commons.codec.digest.DigestUtils
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.AddOntologyAnnotation
import org.semanticweb.owlapi.io.StringDocumentTarget
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.OutputStreamWriter

class OOMFOntologyStorer extends OWLOntologyStorer {

	override
	def canStoreOntology(format: OWLOntologyFormat): Boolean = { 
			return format.isInstanceOf[OOMFOntologyFormat];
	}

	override
	def storeOntology(manager: OWLOntologyManager, ontology: OWLOntology, iri: IRI, format: OWLOntologyFormat): Unit = {
			val mainFile = new File(iri.toURI());
			val dataFolder = OOMFOntologyFormat.dataFolderForFile(mainFile);
			val axiomGroups = this.chunkOntology(ontology);
			FileUtils.cleanDirectory(dataFolder);
			axiomGroups.foreach((group) => writeAxiomGroupToFile(group._1, group._2, dataFolder));
			val shellOntology = this.shellOntologyForOntology(ontology);
			val loadedFormat = ontology.getOWLOntologyManager().getOntologyFormat(ontology);
			val shellFormat = new OWLFunctionalSyntaxOntologyFormat();
			if (loadedFormat.isPrefixOWLOntologyFormat()) {
				shellFormat.copyPrefixesFrom(loadedFormat.asPrefixOWLOntologyFormat());
			}
			val docTarget = new StringDocumentTarget();
			shellOntology.getOWLOntologyManager().saveOntology(shellOntology, shellFormat, docTarget);
			val oomfString = "%oomf%\n" + docTarget.toString();
			val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(mainFile), "UTF-8"));
			writer.write(oomfString);
			writer.close();
	}

	override
	def storeOntology(manager: OWLOntologyManager, ontology: OWLOntology, target: OWLOntologyDocumentTarget, format: OWLOntologyFormat): Unit = {
			this.storeOntology(manager, ontology, target.getDocumentIRI(), format);
	}

	def chunkOntology(ontology: OWLOntology): Map[IRI, Set[OWLAxiom]] = {
			val axiomGroups = new HashMap[IRI, Set[OWLAxiom]]();
			val walker = new OWLOntologyWalker(Set(ontology));
			walker.walkStructure(new OOMFAxiomVisitor(ontology, walker, axiomGroups));
			return axiomGroups;
	}

	def writeAxiomGroupToFile(iri: IRI, axioms: Set[OWLAxiom], parentFolder: File): Unit = {
			val manager = OWLManager.createOWLOntologyManager();
			val filename = filenameForIRI(iri);
			val subFolder = folderForFilename(parentFolder, filename);
			val file = new File(subFolder, filename);
			val ontologyIRI = IRI.create(iri.toString() + "/ontology");
			val groupOntology = manager.createOntology(axioms, ontologyIRI);
			manager.saveOntology(groupOntology, new OWLFunctionalSyntaxOntologyFormat(), IRI.create(file));		
	}

	def folderForFilename(parent: File, filename: String): File = {
			val bytes = DigestUtils.md5(filename);
			return new File(new File(parent, "%02x".format((bytes(0) + 128))), "%02x".format((bytes(1) + 128)));
	}

	def filenameForIRI(iri: IRI): String = {
			return URLEncoder.encode(iri.toString(), "UTF-8") + ".ofn";
	}

	def shellOntologyForOntology(ontology: OWLOntology): OWLOntology = {
			val shellManager = OWLManager.createOWLOntologyManager();
			val shellOntology = shellManager.createOntology(ontology.getOntologyID());
			val ontologyAxioms = ontology.getAnnotations().toList.map((new AddOntologyAnnotation(shellOntology, _)));
			shellManager.applyChanges(ontologyAxioms);
			val ontologyImports = ontology.getImportsDeclarations().toList.map((new AddImport(shellOntology, _)));
			shellManager.applyChanges(ontologyImports);
			return shellOntology;
	}

}