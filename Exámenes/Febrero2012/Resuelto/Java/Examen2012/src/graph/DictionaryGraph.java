/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Class for undirected graphs implemented with a dictionary
 */

package graph;

import java.util.Iterator;

import dictionary.Dictionary;
import dictionary.HashDictionary;
import set.HashSet;
import set.Set;

public class DictionaryGraph<V> implements Graph<V> {
	
	protected Set<V> vertices;               // set with all vertices in graph
	protected Dictionary<V,Set<V>> diEdges;  // Dict with sources as keys and Set of destinations as values
	
	public DictionaryGraph() {
		vertices = new HashSet<V>();
		diEdges = new HashDictionary<V,Set<V>>();
	}
	
	public void addVertex(V v) {
		vertices.insert(v);
	}
	
	private void addDiEdge(V src, V dst) {
/*		if(!vertices.isElem(src))
			throw new GraphException("vertex "+src+" is not in graph");
		if(!vertices.isElem(dst))
			throw new GraphException("vertex "+dst+" is not in graph");
*/		
		vertices.insert(src);
		vertices.insert(dst);

		Set<V> destinations = diEdges.valueOf(src);
		if(destinations == null) {
			destinations = new HashSet<V>();
			diEdges.insert(src, destinations);
		}
		destinations.insert(dst);			
	}	
	
	public void addEdge(V v, V w) {
		addDiEdge(v, w);
		addDiEdge(w, v);		
	}	
	
	private void deleteDiEdge(V src, V dst) {
		Set<V> destinations = diEdges.valueOf(src);
		if(destinations != null) 
			destinations.delete(dst);
	}	
	
	public void deleteEdge(V v, V w) {
		deleteDiEdge(v, w);
		deleteDiEdge(w, v);		
	}	
	
	public void deleteVertex(V v) {
		vertices.delete(v); // remove from set of vertices
		diEdges.delete(v); //remove all edges from v
		// remove all edges to v
		for(V w : vertices) 
			deleteDiEdge(w,v);
	}
	
	public Set<V> vertices() {
		return vertices;
	}	
	
	public int numVertices() {
		return vertices.size();
	}	
	
	public int numEdges() {
		int directedEdges  = 0;
		for(V src : diEdges.keys())
			directedEdges  += successors(src).size();
		return directedEdges  / 2;
	}	
	
	public Set<V> successors(V v) {
		Set<V> destinations = diEdges.valueOf(v);
		return destinations == null ? new HashSet<V>() : destinations;
	}	
	
	public int degree(V v) {
		return successors(v).size();
	}
	
	public String toString() {
		String s = "DictionaryGraph(vertices=(";
		
		Iterator<V> it = vertices.iterator();
		while(it.hasNext()) 
			s += it.next() + (it.hasNext() ? "," : "");
		s += ")";
		
		Set<V> printed = new HashSet<V>();
		int edges = numEdges();
		
		s += ", edges=(";
		for (V v : vertices()) {
			for(V w : successors(v)) 
				if(!printed.isElem(w)){
					edges--;
					s += v + "-" + w + (edges > 0 ? "," : "");
				}
			printed.insert(v);
		}	
		s += "))";
		
		return s;
	}
	
	public Object clone() {
		DictionaryGraph<V> clone = new DictionaryGraph<V>();
		
		for(V v : vertices)
			clone.addVertex(v);
		
		for(V v : vertices)
			for(V w : successors(v))
				clone.addEdge(v, w);
		
		return clone;
	}		
}
