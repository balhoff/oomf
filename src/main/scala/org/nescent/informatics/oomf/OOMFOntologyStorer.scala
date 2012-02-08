package org.nescent.informatics.oomf

import java.io.BufferedWriter
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.net.URLEncoder

import scala.collection.JavaConversions._
import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.io.FileUtils
import org.nescent.informatics.oomf.OOMFOntologyFormat
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.OWLFunctionalSyntaxOntologyFormat
import org.semanticweb.owlapi.io.OWLOntologyDocumentTarget
import org.semanticweb.owlapi.io.StringDocumentTarget
import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.AddOntologyAnnotation
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyFormat
import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.model.OWLOntologyStorer
import org.semanticweb.owlapi.util.DefaultPrefixManager
import org.semanticweb.owlapi.util.OWLOntologyWalker
import org.semanticweb.owlapi.vocab.PrefixOWLOntologyFormat

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
			axiomGroups.foreach((group) => writeAxiomGroupToFile(group._1, group._2, dataFolder, format.asPrefixOWLOntologyFormat()));
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

	def writeAxiomGroupToFile(iri: IRI, axioms: Set[OWLAxiom], parentFolder: File, format: PrefixOWLOntologyFormat): Unit = {
			val manager = OWLManager.createOWLOntologyManager();
			val prefixer = new DefaultPrefixManager(format);
			val file = if (prefixer.getPrefixIRI(iri) == null) {
				val filename = filenameForName(iri.toString());
				new File(folderForFilename(new File(parentFolder, "default"), filename), filename);
			} else {
				val shortName = prefixer.getShortForm(iri);
				val parts = shortName.split(":", 2);
				val prefix = parts(0);
				val filename = filenameForName(parts(1));
				new File(folderForFilename(new File(parentFolder, prefix), filename), filename);
			}
			val ontologyIRI = IRI.create(iri.toString() + "/ontology");
			val groupOntology = manager.createOntology(axioms, ontologyIRI);
			manager.saveOntology(groupOntology, new OWLFunctionalSyntaxOntologyFormat(), IRI.create(file));		
	}

	def folderForFilename(parent: File, filename: String): File = {
			val bytes = DigestUtils.md5(filename);
			return new File(new File(parent, "%02x".format((bytes(0) + 128))), "%02x".format((bytes(1) + 128)));
	}

	def filenameForName(name: String): String = {
			return URLEncoder.encode(name, "UTF-8") + ".ofn";
	}

	//	def filenameForIRI(iri: IRI): String = {
	//			return URLEncoder.encode(iri.toString(), "UTF-8") + ".ofn";
	//	}

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