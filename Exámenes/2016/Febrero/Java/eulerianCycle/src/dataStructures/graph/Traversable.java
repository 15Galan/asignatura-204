/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Interface for a container defining a successors method
 */

package dataStructures.graph;

import dataStructures.set.Set;

/**
 * Interfaces for data structures providing a {@code successors} method, returning
 * all successor elements for an element in the structure. 
 *
 * @param <T> Type of elements store in data structure.
 */
public interface Traversable<T> {
	Set<T> successors(T x); //
} 
