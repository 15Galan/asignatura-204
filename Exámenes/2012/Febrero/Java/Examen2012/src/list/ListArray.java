package list;

import java.util.Arrays;
import java.util.Iterator;

public class ListArray<T> implements List<T> {
	protected int nextFree;
	protected T [] elements;
	private final int CAPACITY = 10;

	public ListArray() {
		elements = (T[]) new Object[CAPACITY];
		nextFree = 0;
	}

	public T get(int i) {
		indexValidation(i);
		return elements[i];	
	}

	public boolean isEmpty() {
		return nextFree == 0;
	}

	public int size() {
		return nextFree;
	}

	public void set(int i, T elem) {
		indexValidation(i);
		elements[i] = elem;
	}

	public void add(int i, T elem) {
		ensureCapacity();
		if (i != nextFree) {
			indexValidation(i);
			for (int pos = nextFree - 1; pos >= i; pos--) {
				elements[pos+1] = elements[pos];
			}
		}
		elements[i] = elem;
		nextFree++;
	}
	
	public void remove(int i) {
		indexValidation(i);
		for (int pos = i; pos < nextFree - 1 ; pos++) {
			elements[pos] = elements[pos+1];
		}
		nextFree--;
	}
	
	private void ensureCapacity() {
	      if (nextFree == elements.length) {
	    	  elements = Arrays.copyOf(elements, nextFree*2);
	      }	
	}
	
	private void indexValidation(int i) {
		if (i < 0 || i >= size()) {
			throw new ListException("Invalid position " + i);
		}
	}	

	//Iterator
	public Iterator<T> iterator() {
		return new ArrayListIterator();
	}

	private class ArrayListIterator implements Iterator<T> {
		int current;

		public ArrayListIterator(){
			current = 0;
		}
		
		public boolean hasNext() {
			return current < nextFree;
		}

		public T next() {
			T x = elements[current];
			current = current + 1;
			return x;
		}

		public void remove() {
			throw new UnsupportedOperationException();			
		}		
	}  

	public String toString() {
		String salida = "List ( ";
		for (int i = 0 ; i < nextFree ; i++) {
			salida += elements[i] + " ";
		}
		return salida + ")";
	}
	
}
