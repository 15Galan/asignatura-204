/**
 * @author Pepe Gallardo, Data Structures, Grado en Inform�tica. UMA.
 *
 * Exceptions for graph operations
 */

package dataStructures.graph;

public class GraphException extends RuntimeException {

	private static final long serialVersionUID = -7807923404308749451L;

	public GraphException() {
	   super();
	 }

	 public GraphException(String msg) {
	   super(msg);
	 }
	}
