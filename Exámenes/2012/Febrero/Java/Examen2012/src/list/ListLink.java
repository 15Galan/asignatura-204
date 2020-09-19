package list;

import java.util.Iterator;


public class ListLink<T> implements List<T> {
	private Node<T> front;
	private Node<T> back;
	private int size;

	private static class Node<D> {
		D data;
		Node<D> next;

		public Node(D data) {
			this(data, null);
		}

		public Node(D data, Node<D> next) {
			this.data = data;
			this.next = next;
		}
	}

	public ListLink() {
		front = null;
		back = null;
		size = 0;
	}

	public boolean isEmpty() {
		return size == 0;
	}

	public int size() {
		return size;
	}

	public void set(int i, T elem) {
		indexValidation(i);
		Node<T> aux = front;
		for (int pos = 0; pos < i; pos++) {
			aux = aux.next;
		}
		aux.data = elem;
	}

	public T get(int i) {
		indexValidation(i);
		Node<T> aux = front;
		for (int pos = 0; pos < i; pos++) {
			aux = aux.next;
		}
		return aux.data;
	}

	public void add(int i, T elem) {
		if (i == size) {
			Node<T> newNode = new Node<T>(elem);
			if (size != 0) {
				back.next = newNode;
			} else {
				front = newNode;
			}
			back = newNode;
		} else if (i == 0) {
			front = new Node<T>(elem, front);
		} else {
			indexValidation(i);
			Node<T> aux = front;
			for (int pos = 0; pos < i - 1; pos++) {
				aux = aux.next;
			}
			aux.next = new Node<T>(elem, aux.next);
		}
		size++;
	}

	public void remove(int i) {
		if (i == 0) {
			front = front.next;
			if (front == null) {
				back = null;
			}
		} else {
			indexValidation(i);
			Node<T> aux = front;
			for (int pos = 0; pos < i - 1; pos++) {
				aux = aux.next;
			}
			if (i == (size - 1)) {
				back = aux;
				back.next = null;
			} else {
				aux.next = aux.next.next;
			}
		}
		size--;
	}

	private void indexValidation(int i) {
		if (i < 0 || i >= size()) {
			throw new ListException("Invalid position " + i);
		}
	}

	//Iterator
	public Iterator<T> iterator() {
		return new LinkedListIterator();
	}

	private class LinkedListIterator implements Iterator<T> {
		Node<T> current;

		public LinkedListIterator(){
			current = front;
		}
		
		public boolean hasNext() {
			return current != null;
		}

		public T next() {
			T x = current.data;
			current = current.next;
			return x;
		}

		public void remove() {
			throw new UnsupportedOperationException();			
		}		
	}
	
	public String toString() {
		String salida = "List (";
		Node<T> aux = front;
		while (aux != null) {
			salida += aux.data+ " ";
			aux = aux.next;
		}
		return salida + ")";
	}
}
