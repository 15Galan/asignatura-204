/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Interface for queues
 */
 
package dataStructures.queue;


/**
 * Interface for First In First Out data structures.
 *
 * @param <T> Type of elements in stack.
 */
public interface Queue<T> {
	/** 
	 * Test for queue emptiness. 
	 * @return {@code true} if queue is empty, else {@code false}.
	 */
	boolean isEmpty();
	
	/**
	 * Inserts new element at rear (end) of queue.
	 * @param x the element to enqueue.
	 */
	void enqueue(T x);
	
	/** 
	 * Retrieves (without removing) first element in queue.
	 * @throws EmptyQueueException if queue is empty.
	 * @return First element in queue.
	 */
	T first();
	
	/**
	 * Removes first element from queue.
	 * @throws EmptyQueueException if queue is empty.
	 */
	void dequeue();
}
