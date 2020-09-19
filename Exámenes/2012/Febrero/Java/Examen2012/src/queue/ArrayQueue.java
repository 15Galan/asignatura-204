
package queue;

public class ArrayQueue<T> implements Queue<T> {
	protected T[] elements;
	protected int first, last, size;

	private final int INITIAL_CAPACITY = 10;

	public ArrayQueue() {
		elements = (T[]) new Object[INITIAL_CAPACITY];
		size = 0;
		first = -1;
		last = -1;
	}
	
	private int advance(int i) {
		return (i+1) % elements.length;
	}
	
	private void ensureCapacity() {
		if (size == elements.length) {
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

  // implements circular increment
	public boolean isEmpty() {
		return size == 0;
	}

	public void enqueue(T x) {
		ensureCapacity();
		last = advance(last);
		elements[last] = x;
		if (isEmpty())
			first = last;
		size++;
	}

	public T first() {
		if (isEmpty())
			throw new EmptyQueueException("first on empty queue");
		else
			return elements[first];
	}

	public void dequeue() {
		if (isEmpty())
			throw new EmptyQueueException("dequeue on empty queue");
		else {
			first = advance(first);
			if (size == 1) {
				last = first;
			}
			size--;
		}
	}
	
	@Override public String toString() {
		String s = "ArrayQueue(";
		int f = first;
		for(int i=0; i<size; i++) { 
			s += elements[f] + (i<size-1 ? "," : "");
			f = advance(f);
		}	
		s += ")";
		return s;			
	}	
}
