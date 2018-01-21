/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Common interface for all graphs
 */

package graph;

import set.Set;

public interface Graph<V> extends Traversable<V>, Cloneable {

	void addVertex(V v);
	
	void deleteVertex(V v);

	void addEdge(V v, V w);

	void deleteEdge(V v, V w);
	
	int numVertices();

	int numEdges();
	
	Set<V> vertices();

	int degree(V v);	
	
	Object clone();
	
}
