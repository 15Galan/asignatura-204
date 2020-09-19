/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Queue implementation using an array
 */
 
package dataStructures.queue;

/**
 * Queue implemented using an array of elements. Size of array 
 * (capacity) is automatically 
 * increased when it runs out of capacity.
 * @param <T> Type of elements in queue.
 */
public class ArrayQueue<T> implements Queue<T> {
	protected T[] elements;
	protected int first, last, size;

	private static final int DEFAULT_INITIAL_CAPACITY = 128;

	/**
	 * Creates an empty queue. Initial capacity is {@code n} elements.
     * Capacity is automatically increased when needed.
     * @param n Initial capacity.
	 * <p>Time complexity: O(1)
	 */
	@SuppressWarnings("unchecked")
	public ArrayQueue(int n) {
		elements = (T[]) new Object[n];
		size = 0;
		first = 0;
		last = n - 1;
	}
	
	/**
	 * Creates an empty queue with default initial capacity.
     * Capacity is automatically increased when needed.
	 * <p>Time complexity: O(1)
	 */
	public ArrayQueue() {
		this(DEFAULT_INITIAL_CAPACITY);
	}	
		
	// circular increment
	private int advance(int i) {
		return (i+1) % elements.length;
	}
	
	private void ensureCapacity() {
		if (size == elements.length) {
			@SuppressWarnings("unchecked")
			T[] extension = (T[]) new Object[2 * elements.length];
			for(int i=0; i<size; i++) {
				extension[i] = elements[first];
				first = advance(first);
			}			
			elements = extension;
			first = 0;
			last = size-1;
		}
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public boolean isEmpty() {
		return size == 0;
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: mostly O(1). O(n) when queue capacity has to be increased.
	 */
	public void enqueue(T x) {
		ensureCapacity();
		last = advance(last);
		elements[last] = x;
		size++;
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 * @throws EmptyQueueException {@inheritDoc} 
	 */
	public T first() {
		if (isEmpty())
			throw new EmptyQueueException("first on empty queue");
		else
			return elements[first];
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 * @throws EmptyQueueException {@inheritDoc} 
	 */
	public void dequeue() {
		if (isEmpty())
			throw new EmptyQueueException("dequeue on empty queue");
		else {
			first = advance(first);
			size--;
		}
	}
	
	/** 
	 * Returns representation of queue as a String.
	 */
  	@Override public String toString() {
    String className = getClass().getSimpleName();
		String s = className+"(";
		int f = first;
		for(int i=0; i<size; i++) { 
			s += elements[f] + (i<size-1 ? "," : "");
			f = advance(f);
		}	
		s += ")";
		return s;			
	}	
}
