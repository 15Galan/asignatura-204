/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Class for directed graphs implemented with a dictionary
 */

package dataStructures.graph;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.set.HashSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

import java.util.Iterator;

public class WeightedDictionaryDiGraph<V,W> implements WeightedDiGraph<V,W> {

	protected Set<V> vertices;               // set with all vertices in graph
	protected Dictionary<V,Set<Tuple2<V,W>>> diEdges;  // Dict with sources as keys and Set of destinations as values

	public WeightedDictionaryDiGraph() {
		vertices = new HashSet<>();
		diEdges = new HashDictionary<>();
	}

	public WeightedDictionaryDiGraph(Set<V> listVert, List<WDiEdge<V,W>> edges) {
		this();
		for (V vertice: listVert) {
			addVertex(vertice);
		}
		for (WDiEdge<V,W> edge: edges) {
			addDiEdge(edge.getSrc(),edge.getWeight(), edge.getDst());
		}
	}

	public boolean isEmpty() {
		return vertices.isEmpty();
	}

	public void addVertex(V v) {
		vertices.insert(v);
	}
	
	public void addDiEdge(V src, W w, V dst) {
		if(!vertices.isElem(src))
			throw new GraphException("vertex "+src+" is not in graph");
		if(!vertices.isElem(dst))
			throw new GraphException("vertex "+dst+" is not in graph");
		
		Set<Tuple2<V,W>> destinations = diEdges.valueOf(src);
		if(destinations == null) {
			destinations = new HashSet<>();
			diEdges.insert(src, destinations);
		}
		destinations.insert(new Tuple2<>(dst,w));
	}	
	
	public void deleteDiEdge(V src, W w, V dst) {
		Set<Tuple2<V,W>> destinations = diEdges.valueOf(src);
		if(destinations != null)
			destinations.delete(new Tuple2<>(dst,w));
	}	

	public void deleteVertex(V x) {
		vertices.delete(x); // remove from set of vertices
		diEdges.delete(x); //remove all edges from v
		// remove all edges to v
		for(V y: vertices)
			for (W w : weightedEdges(y, x))
				deleteDiEdge(y, w, x);
	}

	private Set<W> weightedEdges(V src, V dst) {
		Set<W> set = new HashSet<>();
		Set<Tuple2<V,W>> destinations = diEdges.valueOf(src);
		if (destinations != null) {
			for (Tuple2<V, W> tuple : destinations) {
				if (tuple._1().equals(dst)) {
					set.insert(tuple._2());
				}
			}
		}
		return set;
	}
	public Set<V> vertices() {
		return vertices;
	}

	@Override
	public List<WDiEdge<V, W>> wDiEdges() {
		List<WDiEdge<V,W>> list = new LinkedList<>();
		for (V vertice: diEdges.keys()) {
			for (Tuple2<V,W> tuple: diEdges.valueOf(vertice)) {
				list.append(new WDiEdge<>(vertice, tuple._2(), tuple._1()));
			}
		}
		return list;
	}

	public int numVertices() {
		return vertices.size();
	}	
	
	public int numEdges() {
		int sz = 0;
		for(V src : diEdges.keys())
			sz += diEdges.valueOf(src).size();
		return sz;
	}	
	
	public Set<Tuple2<V,W>> successors(V src) {
		Set<Tuple2<V,W>> destinations = diEdges.valueOf(src);
		return destinations == null ? new HashSet<>() : destinations;
	}

	public Set<Tuple2<V,W>> predecessors(V dst) {
		Set<Tuple2<V,W>> sources = new HashSet<>();
		for(V src : vertices)
			for(W w : weightedEdges(src, dst))
				sources.insert(new Tuple2<>(src,w));
		return sources;
	}
	
	public int inDegree(V v) {
		return predecessors(v).size();
	}

	public int outDegree(V v) {
		return successors(v).size();
	}

	public String toString() {
    	String className = getClass().getSimpleName();
		String s = className+"(vertices=(";

		Iterator<V> it = vertices.iterator();
		while(it.hasNext()) 
			s += it.next() + (it.hasNext() ? "," : "");
		s += ")";
		
		int edges = numEdges();
		
		s += ", edges=(";
		for (V v : vertices())
			for(Tuple2<V,W>  tuple : successors(v)) {
				edges--;
				s += v + "-" + tuple._2() + "->" + tuple._1() + (edges > 0 ? "," : "");
			}
		s += "))";
		
		return s;
	}	

	public Object clone() {
		WeightedDictionaryDiGraph<V,W> clone = new WeightedDictionaryDiGraph<>();

		for(V v : vertices)
			clone.addVertex(v);
		
		for(V v : vertices)
			for(Tuple2<V,W> tuple : successors(v))
				clone.addDiEdge(v, tuple._2(),tuple._1());
			
		return clone;
	}
}
