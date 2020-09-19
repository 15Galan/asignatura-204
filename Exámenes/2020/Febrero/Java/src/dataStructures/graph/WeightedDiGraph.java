/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Interface for weighted directed graphs
 */

package dataStructures.graph;

import dataStructures.list.List;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

public interface WeightedDiGraph<V,W>  extends WTraversable<V,W>, Cloneable {
	boolean isEmpty();

	void addVertex(V x);
	
	void deleteVertex(V x);

	void addDiEdge(V x, W w, V y);

	void deleteDiEdge(V x, W w, V y);
	
	int numVertices();

	int numEdges();
	
	Set<V> vertices();

	List<WDiEdge<V,W>> wDiEdges();

	int inDegree(V v);
	
	int outDegree(V v);

	Set<Tuple2<V,W>> predecessors(V dst);

	Object clone();
}