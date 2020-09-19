/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Common interface for all graphs
 */

package dataStructures.graph;

import dataStructures.set.Set;

/**
 * Interface for undirected graphs.
 *
 * @param <V> Type of vertices in graph.
 */
public interface Graph<V> extends Traversable<V>, Cloneable {
	/**
	 * true if graph has no vertices.
	 */
	boolean isEmpty();

	/**
	 * Inserts a vertex in the graph. Vertices must be inserted
	 * into graph prior to adding incident edges.
	 *  
	 * @param v Vertex to add to graph.
	 */
	void addVertex(V v);
	
	/**
	 * Removes one vertex from the graph. It also removes all edges
	 * that are incident to removed vertex.
	 * @param v Vertex to remove.
	 */
	void deleteVertex(V v);

	
	/**
	 * Adds an edge to the graph. Note that vertices should be in
	 * 
	 * @param v
	 * @param w
	 */
	void addEdge(V v, V w);

	void deleteEdge(V v, V w);
	
	int numVertices();

	int numEdges();
	
	Set<V> vertices();

	int degree(V v);	
	
	Object clone();
	
}
