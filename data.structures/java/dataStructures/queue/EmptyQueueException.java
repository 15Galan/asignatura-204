/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Exceptions for queues
 */
 
package dataStructures.queue;

public class EmptyQueueException extends RuntimeException {

	private static final long serialVersionUID = -7341344657382375385L;

	public EmptyQueueException() {
		super();
	}

	public EmptyQueueException(String msg) {
		super(msg);
	}
}
