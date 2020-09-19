/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Interface for a container defining a successors method
 */

package graph;

import set.Set;

public interface Traversable<T> {
	Set<T> successors(T x);
}
