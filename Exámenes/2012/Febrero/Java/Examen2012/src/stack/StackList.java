package stack;

import java.util.*;

public class StackList<T> implements Stack<T> {
	protected LinkedList<T> elements;

	public StackList() {
		elements = new LinkedList<T>();
	}
	
	public StackList(Stack<T> st) {
		Stack<T> stAux = new StackList<T>();
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
		elements.addFirst(elem);
	}

	public boolean isEmpty() {
		return elements.isEmpty();
	}

	public T top() {
		if (isEmpty()) {
			throw new EmptyStackException("pop: empty stack");
		}
		return elements.getFirst();
	}

	public void pop() {
		if (isEmpty()) {
			throw new EmptyStackException("pop: empty stack");
		}
		elements.removeFirst();
	}
}
