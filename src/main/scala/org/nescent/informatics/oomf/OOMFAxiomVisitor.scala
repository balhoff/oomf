package org.nescent.informatics.oomf


import scala.collection.JavaConversions._
import scala.collection.mutable.Map
import scala.collection.mutable.Set

import org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom
import org.semanticweb.owlapi.model.SWRLRule
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom
import org.semanticweb.owlapi.model.OWLAnonymousIndividual
import org.semanticweb.owlapi.model.OWLObjectPropertyCharacteristicAxiom
import org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom
import org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom
import org.semanticweb.owlapi.model.OWLDisjointUnionAxiom
import org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom
import org.semanticweb.owlapi.model.OWLAnnotationPropertyRangeAxiom
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom
import org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom
import org.semanticweb.owlapi.model.OWLDeclarationAxiom
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom
import org.semanticweb.owlapi.model.OWLDatatypeDefinitionAxiom
import org.semanticweb.owlapi.util.ObjectPropertySimplifier
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom
import org.semanticweb.owlapi.model.OWLDataPropertyDomainAxiom
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
import org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom
import org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom
import org.semanticweb.owlapi.model.OWLAnnotationPropertyDomainAxiom
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom
import org.semanticweb.owlapi.model.OWLSymmetricObjectPropertyAxiom
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
import org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom
import org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom
import org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom
import org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLObjectInverseOf
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom
import org.semanticweb.owlapi.util.OWLOntologyWalker
import org.semanticweb.owlapi.util.OWLOntologyWalkerVisitor
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom
import org.semanticweb.owlapi.model.OWLSameIndividualAxiom
import org.semanticweb.owlapi.model.OWLSubAnnotationPropertyOfAxiom
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLNegativeDataPropertyAssertionAxiom
import org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom
import org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom

class OOMFAxiomVisitor(ontology: OWLOntology, walker: OWLOntologyWalker, axiomGroups: Map[IRI, Set[OWLAxiom]]) extends OWLOntologyWalkerVisitor[Unit](walker) {

	val factory = ontology.getOWLOntologyManager().getOWLDataFactory();
	val simplifier = new ObjectPropertySimplifier(factory);
	val chooseIRI = (a: IRI, b: IRI) => (if (a.toString() < b.toString()) a else b);

	override
	def visit(axiom: OWLAnnotationAssertionAxiom): Unit = {
			val iri = if (axiom.getSubject().isInstanceOf[OWLAnonymousIndividual] || axiom.getValue().isInstanceOf[OWLAnonymousIndividual]) {
				OOMFOntologyFormat.AnonymousIRI;
			} else {
				axiom.getSubject().asInstanceOf[IRI];
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLAnnotationPropertyDomainAxiom): Unit = {
			this.addAxiom(axiom.getProperty().getIRI(), axiom);
	}

	override
	def visit(axiom: OWLAnnotationPropertyRangeAxiom): Unit = {
			this.addAxiom(axiom.getProperty().getIRI(), axiom);
	}

	override
	def visit(axiom: OWLAsymmetricObjectPropertyAxiom): Unit = {
			this.visitPropertyCharacteristicAxiom(axiom);
	}

	override
	def visit(axiom: OWLClassAssertionAxiom): Unit = {
			val iri = axiom.getIndividual() match {
			case named: OWLNamedIndividual => named.getIRI();
			case anonymous: OWLAnonymousIndividual => OOMFOntologyFormat.AnonymousIRI;
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLDataPropertyAssertionAxiom): Unit = {
			val iri = axiom.getSubject() match {
			case named: OWLNamedIndividual => named.getIRI();
			case anonymous: OWLAnonymousIndividual => OOMFOntologyFormat.AnonymousIRI;
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLDataPropertyDomainAxiom): Unit = {
			this.addAxiom(axiom.getProperty().asOWLDataProperty().getIRI(), axiom);
	}

	override
	def visit(axiom: OWLDataPropertyRangeAxiom): Unit = {
			this.addAxiom(axiom.getProperty().asOWLDataProperty().getIRI(), axiom);
	}

	override
	def visit(axiom: OWLDatatypeDefinitionAxiom): Unit = {
			this.addAxiom(axiom.getDatatype().getIRI(), axiom);
	}

	override
	def visit(axiom: OWLDeclarationAxiom): Unit = {
			this.addAxiom(axiom.getEntity().getIRI(), axiom);
	}

	override
	def visit(axiom: OWLDifferentIndividualsAxiom): Unit = {
			val iri = if (axiom.getIndividuals().exists(_.isAnonymous())) {
				OOMFOntologyFormat.AnonymousIRI;
			} else {
				axiom.getIndividuals().map(_.asOWLNamedIndividual().getIRI()).reduce(chooseIRI);
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLDisjointClassesAxiom): Unit = {
			val iri = if (axiom.getClassExpressions().exists(!_.isAnonymous())) {
				axiom.getClassExpressions().filter(!_.isAnonymous()).map(_.asOWLClass().getIRI()).reduce(chooseIRI);
			} else {
				OOMFOntologyFormat.GCIsIRI;
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLDisjointDataPropertiesAxiom): Unit = {
			val iri = axiom.getProperties().map(_.asOWLDataProperty().getIRI()).reduce(chooseIRI);
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLDisjointObjectPropertiesAxiom): Unit = {
			val iri = axiom.getProperties().map(this.findPropertyIRI(_)).reduce(chooseIRI);
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLDisjointUnionAxiom): Unit = {
			val iri = if (axiom.getClassExpressions().exists(!_.isAnonymous())) {
				axiom.getClassExpressions().filter(!_.isAnonymous()).map(_.asOWLClass().getIRI()).reduce(chooseIRI);
			} else {
				OOMFOntologyFormat.GCIsIRI;
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLEquivalentClassesAxiom): Unit = {
			val iri = if (axiom.getClassExpressions().exists(!_.isAnonymous())) {
				axiom.getClassExpressions().filter(!_.isAnonymous()).map(_.asOWLClass().getIRI()).reduce(chooseIRI);
			} else {
				OOMFOntologyFormat.GCIsIRI;
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLEquivalentDataPropertiesAxiom): Unit = {
			val iri = axiom.getProperties().map(_.asOWLDataProperty().getIRI()).reduce(chooseIRI);
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLEquivalentObjectPropertiesAxiom): Unit = {
			val iri = axiom.getProperties().map(this.findPropertyIRI(_)).reduce(chooseIRI);
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLFunctionalDataPropertyAxiom): Unit = {
			this.addAxiom(axiom.getProperty().asOWLDataProperty().getIRI(), axiom);
	}

	override
	def visit(axiom: OWLFunctionalObjectPropertyAxiom): Unit = {
			this.visitPropertyCharacteristicAxiom(axiom);
	}

	override
	def visit(axiom: OWLInverseFunctionalObjectPropertyAxiom): Unit = {
			this.visitPropertyCharacteristicAxiom(axiom);
	}

	override
	def visit(axiom: OWLInverseObjectPropertiesAxiom): Unit = {
			val iri = this.findPropertyIRI(axiom.getFirstProperty());
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLIrreflexiveObjectPropertyAxiom): Unit = {
			this.visitPropertyCharacteristicAxiom(axiom);
	}

	override
	def visit(axiom: OWLNegativeDataPropertyAssertionAxiom): Unit = {
			val iri = axiom.getSubject() match {
			case named: OWLNamedIndividual => named.getIRI();
			case anonymous: OWLAnonymousIndividual => OOMFOntologyFormat.AnonymousIRI;
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLNegativeObjectPropertyAssertionAxiom): Unit = {
			val iri = if (axiom.getSubject().isInstanceOf[OWLAnonymousIndividual] || axiom.getObject().isInstanceOf[OWLAnonymousIndividual]) {
				OOMFOntologyFormat.AnonymousIRI;
			} else {
				axiom.getSubject().asOWLNamedIndividual().getIRI();
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLObjectPropertyAssertionAxiom): Unit = {
			val iri = if (axiom.getSubject().isInstanceOf[OWLAnonymousIndividual] || axiom.getObject().isInstanceOf[OWLAnonymousIndividual]) {
				OOMFOntologyFormat.AnonymousIRI;
			} else {
				axiom.getSubject().asOWLNamedIndividual().getIRI();
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLObjectPropertyDomainAxiom): Unit = {
			this.addAxiom(this.findPropertyIRI(axiom.getProperty()), axiom);
	}

	override
	def visit(axiom: OWLObjectPropertyRangeAxiom): Unit = {
			this.addAxiom(this.findPropertyIRI(axiom.getProperty()), axiom);
	}

	override
	def visit(axiom: OWLReflexiveObjectPropertyAxiom): Unit = {
			this.visitPropertyCharacteristicAxiom(axiom);
	}

	override
	def visit(axiom: OWLSameIndividualAxiom): Unit = {
			val iri = if (axiom.getIndividuals().exists(_.isAnonymous())) {
				OOMFOntologyFormat.AnonymousIRI;
			} else {
				axiom.getIndividuals().map(_.asOWLNamedIndividual().getIRI()).reduce(chooseIRI);
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLSubAnnotationPropertyOfAxiom): Unit = {
			this.addAxiom(axiom.getSubProperty().getIRI(), axiom);
	}

	override
	def visit(axiom: OWLSubClassOfAxiom): Unit = {
			val iri = axiom.getSubClass().isAnonymous() match {
			case true => OOMFOntologyFormat.GCIsIRI;
			case false => axiom.getSubClass().asOWLClass().getIRI();
			}
			this.addAxiom(iri, axiom);
	}

	override
	def visit(axiom: OWLSubDataPropertyOfAxiom): Unit = {
			this.addAxiom(axiom.getSubProperty().asOWLDataProperty().getIRI(), axiom);
	}

	override
	def visit(axiom: OWLSubObjectPropertyOfAxiom): Unit = {
			this.addAxiom(this.findPropertyIRI(axiom.getSubProperty()), axiom);
	}

	override
	def visit(axiom: OWLSubPropertyChainOfAxiom): Unit = {
			this.addAxiom(this.findPropertyIRI(axiom.getSuperProperty()), axiom);
	}

	override
	def visit(axiom: OWLSymmetricObjectPropertyAxiom): Unit = {
			this.visitPropertyCharacteristicAxiom(axiom);
	}

	override
	def visit(axiom: OWLTransitiveObjectPropertyAxiom): Unit = {
			this.visitPropertyCharacteristicAxiom(axiom);
	}

	override
	def visit(rule: SWRLRule): Unit = {
			this.addAxiom(OOMFOntologyFormat.RulesIRI, rule);
	}

	def visitPropertyCharacteristicAxiom(axiom: OWLObjectPropertyCharacteristicAxiom): Unit = {
			val iri: IRI = simplifier.getSimplified(axiom.getProperty()) match {
			case namedProperty: OWLObjectProperty => namedProperty.getIRI();
			case inverse: OWLObjectInverseOf => inverse.getInverse().asOWLObjectProperty().getIRI();
	};
	this.addAxiom(iri, axiom);
	}

	def addAxiom(iri: IRI, axiom: OWLAxiom) = {
		if (!axiomGroups.keySet.contains(iri)) axiomGroups += (iri -> Set[OWLAxiom]());
		axiomGroups(iri) += axiom;
	}

	def findPropertyIRI(property: OWLObjectPropertyExpression): IRI = {
		return simplifier.getSimplified(property) match {
		case namedProperty: OWLObjectProperty => namedProperty.getIRI();
		case inverse: OWLObjectInverseOf => inverse.getInverse().asOWLObjectProperty().getIRI();
		};
	}

}