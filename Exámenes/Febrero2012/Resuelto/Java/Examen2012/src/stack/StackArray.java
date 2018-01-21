package stack;
import java.util.*;

public class StackArray<T> implements Stack<T> {
	protected int nextFree;
	protected T[] elements;
	private final int CAPACITY = 10;

	public StackArray() {
		elements = (T[]) new Object[CAPACITY];
		nextFree = 0;
	}

	public StackArray(Stack<T> st) {
		this();
		Stack<T> stAux = new StackArray<T>();
		while (!st.isEmpty()) {
			stAux.push(st.top());
			st.pop();
		}
		while (!stAux.isEmpty()) {
			this.push(stAux.top());
			st.push(stAux.top());
			stAux.pop();
		}
	}
	
	public void push(T elem) {
		ensureCapacity();
		elements[nextFree] = elem;
		nextFree++;
	}

	public boolean isEmpty() {
		return nextFree == 0;
	}

	public T top() {
		if (isEmpty()){
			throw new EmptyStackException("pop: empty stack");
		}
		return elements[nextFree - 1];
	}

	public void pop() {
		if (isEmpty()){
			throw new EmptyStackException("pop: empty stack");
		}
		nextFree--;
	}
	public void ensureCapacity() {
	      if (nextFree == elements.length) {
	    	  elements = Arrays.copyOf(elements, nextFree*2);
	      }	
	}
	
	public String toString() {
		String salida = "Stack ( ";
		for (int i = nextFree-1 ; i >= 0 ; i--) {
			salida += elements[i] + " ";
		}
		return salida + ")";
	}

}
