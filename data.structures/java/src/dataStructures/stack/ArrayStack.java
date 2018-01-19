/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Stack implementation using an array
 */
 
package dataStructures.stack;

import java.util.Arrays;

/**
 * Stack implemented using an array of elements. Size of array
 * (capacity) is automatically increased when it runs out of capacity.
 * @param <T> Type of elements in stack. 
 */
public class ArrayStack<T> implements Stack<T> {

	protected T[] elements;
	protected int nextFree;
	
	private static final int DEFAULT_INITIAL_CAPACITY = 128;
	
	/**
	 * Creates an empty stack. Initial capacity is {@code n} elements.
   * Capacity is automatically increased when needed.
	 * @param n Initial capacity.
	 * <p>Time complexity: O(1)
	 */
	@SuppressWarnings("unchecked")
	public ArrayStack(int n) {
		elements = (T[]) new Object[n];
		nextFree = 0;
	}

	/**
	 * Creates an empty stack with default initial capacity.
   * Capacity is automatically increased when needed.
	 * <p>Time complexity: O(1)
	 */
	public ArrayStack() {
		this(DEFAULT_INITIAL_CAPACITY);
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public boolean isEmpty() {
		return nextFree == 0;
	}

	private void ensureCapacity() {
    if (nextFree >= elements.length) {
    	elements = Arrays.copyOf(elements, 2*elements.length);
    }	
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: mostly O(1). O(n) when stack capacity has to be increased. 
	 */
	public void push(T x) {
		ensureCapacity();
		elements[nextFree] = x;
		nextFree++;
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 * @throws EmptyStackException {@inheritDoc} 
	 */
	public T top() {
		if (isEmpty()){
			throw new EmptyStackException("top on empty stack");
		}
		else
			return elements[nextFree-1];
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 * @throws EmptyStackException {@inheritDoc} 
	 */
	public void pop() {
		if (isEmpty()){
			throw new EmptyStackException("pop on empty stack");
		}
		else
			nextFree--;
	}
	
	/** 
	 * Returns representation of stack as a String.
	 */
	@Override public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);  
		String s = className+"(";
		for(int i=nextFree-1; i>=0; i--) 
			s += elements[i] + (i>0 ? "," : "");
		s += ")";
		return s;			
	}
}
