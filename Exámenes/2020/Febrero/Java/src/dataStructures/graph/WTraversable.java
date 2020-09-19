/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Interface for a weighted container defining a successors method
 */

package dataStructures.graph;

import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

/**
 * Interfaces for data structures providing a {@code successors} method, returning
 * all successor elements for an element in the structure. 
 *
 * @param <T> Type of elements store in data structure.
 */
public interface WTraversable<T,W> {
	Set<Tuple2<T,W>> successors(T x); //
} 
