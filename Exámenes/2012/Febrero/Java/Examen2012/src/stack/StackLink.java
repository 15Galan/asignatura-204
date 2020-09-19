package stack;

import java.util.*;

public class StackLink<T> implements Stack<T> {
	protected Node<T> top;

	public StackLink() {
	}
	
	
	public StackLink(Stack<T> st) {
		Stack<T> stAux = new StackLink<T>();
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
		top = new Node<T>(elem, top);
	}

	public boolean isEmpty() {
		return top == null;
	}

	public T top() {
		if (isEmpty()) {
			throw new EmptyStackException("pop: empty stack");
		}
		return top.data;
	}

	public void pop() {
		if (isEmpty()) {
			throw new EmptyStackException("pop: empty stack");
		}
		top = top.next;
	}

	static private class Node<S> {
		S data;
		Node<S> next;

		public Node(S elem, Node<S> n) {
			data = elem;
			next = n;
		}
	}
	
	public String toString() {
		String salida = "Stack (";
		Node<T> aux = top;
		while (aux != null) {
			salida += aux.data+ " ";
			aux = aux.next;
		}
		return salida + ")";
	}
}
