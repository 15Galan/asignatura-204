package queue;


public class QueueLink<T> implements Queue<T> {
	protected Node<T> first;
	protected Node<T> last;

	public void enqueue(T elem) {
		Node<T> aux = new Node<T>(elem, null);
		if (isEmpty()) {
			first = aux;
		} else {
			last.next = aux;	
		}
		last = aux;
	}

	public boolean isEmpty() {
		return first == null;
	}

	public T first() {
		if (isEmpty()) {
			throw new EmptyQueueException("first: empty queue");
		}
		return first.data;
	}

	public void dequeue() {
		if (isEmpty()) {
			throw new EmptyQueueException("dequeue: empty queue");
		}
		first = first.next;
		if (first == null) {
			last = null;
		}
	}

	static protected class Node<S> {
		S data;
		Node<S> next;

		public Node(S elem, Node<S> n) {
			data = elem;
			next = n;
		}
	}
	
	public String toString() {
		String salida = "Queue ( ";
		Node<T> aux = first;
		while (aux != null) {
			salida += aux.data+ " ";
			aux = aux.next;
		}
		return salida + ")";
	}
}
