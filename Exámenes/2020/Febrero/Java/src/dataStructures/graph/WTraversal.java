/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Abstract class implementing generic traversals on a graph
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
import java.util.NoSuchElementException;


public abstract class WTraversal<V,W> {

	private WTraversable<V,W> graph; // graph to explore
	private V source;             // initial node (source) for exploration

	abstract public Store<WDiEdge<V,W>> newStore(); // will be defined in subclasses to return different kinds of stores

	public WTraversal(WTraversable<V,W> g, V src) {
		graph = g;
		source = src;
	}	

	private abstract class BaseIterator {
		protected Set<V> visited;             // already visited vertices 
		protected Store<WDiEdge<V,W>> store;     // diEdges to vertices yet to be explored
		protected Dictionary<V,WDiEdge<V,W>> sources;    // source for each visited vertex during traversal
		protected WDiEdge<V,W> wdedge;               // next vertex to be visited (or null if end of traversal)

		public BaseIterator() {
			visited = new HashSet<>();
			store = newStore();
			store.insert(new WDiEdge<>(source,null,source));
			sources = new HashDictionary<>();
			advanceTraversal();		
		}
	
		// finds next vertex to be visited in traversal. Leaves this vertex (or null at end of traversal) in nextVertex
		protected void advanceTraversal() {
			wdedge = null;
			while(!store.isEmpty() && wdedge ==null) {
				WDiEdge<V,W> edge = store.extract();
				V v = edge.getDst();
				if(!visited.isElem(v)) {
					wdedge = edge;
					visited.insert(v);
					sources.insert(v, wdedge);
					for(Tuple2<V,W> tuple : graph.successors(v))
						if(!visited.isElem(tuple._1()))
							store.insert(new WDiEdge<>(v, tuple._2(),tuple._1()));
				}
			}
		}	

		public boolean hasNext() {
			return wdedge != null;
		}			
	}
	
	
	private class VerticesIterator extends BaseIterator implements Iterator<V> {
		public V next() {
			if (!hasNext())
				throw new NoSuchElementException();	
			
			V vertex = wdedge.getDst();
			
			advanceTraversal(); //for next iteration of iterator
			
			return vertex;	
		}
	}	
	
	public Iterator<V> verticesIterator() {
		return new VerticesIterator();
	}
	
	public Iterable<V> vertices() {		
		return new Iterable<V>(){
			public Iterator<V> iterator() {
				return verticesIterator();
			}
		};
	}	
  
	private class PathsIterator extends BaseIterator implements Iterator<Iterable<WDiEdge<V,W>>> {
		// returns path from initial source to vertex v
		private List<WDiEdge<V,W>> pathTo(WDiEdge<V,W> we) {
			List<WDiEdge<V,W>> path = new LinkedList<>();
			while(we.getSrc()!=source) {
				path.insert(0, we);
				we = sources.valueOf(we.getSrc());
			}
			path.insert(0, we);
			return path;
		}		
		
		public Iterable<WDiEdge<V,W>> next() {
			if (!hasNext())
				throw new NoSuchElementException();	
	
			// reconstruct path from source to visited vertex
			Iterable<WDiEdge<V,W>> path = pathTo(wdedge);
			
			advanceTraversal(); //for next iteration of iterator
			
			return path;
		}
	}	

	public Iterator<Iterable<WDiEdge<V,W>>> pathsIterator() {
		return new PathsIterator();
	}	
	
	public Iterable<Iterable<WDiEdge<V,W>>> paths() {
		return new Iterable<Iterable<WDiEdge<V,W>>>(){
			public Iterator<Iterable<WDiEdge<V,W>>> iterator() {
				return pathsIterator();
			}
		};
	}

	public List<WDiEdge<V,W>> pathTo(V dst) {
		Iterator<Iterable<WDiEdge<V,W>>> pathsIt = paths().iterator();
		Iterable<WDiEdge<V,W>> path = null;
		while (path  == null && pathsIt.hasNext()) {
			Iterable<WDiEdge<V,W>> onePath = pathsIt.next();
			Iterator<WDiEdge<V,W>> onePathIt = onePath.iterator();
			WDiEdge<V,W> wdedge = null;
			while (onePathIt.hasNext()) {
				wdedge = onePathIt.next();
			}
			if (wdedge != null && wdedge.getDst().equals(dst)) {
				path = onePath;
			}
		}

		List<WDiEdge<V,W>>  list = null;
		if (path != null) {
			list = new LinkedList<>();
			for (WDiEdge<V, W> wdedge : path) {
				list.append(wdedge);
			}
		}
		return list;
	}
	
}
