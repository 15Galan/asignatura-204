package queue;

import java.util.*;

public class QueueList<T> implements Queue<T> {
	protected LinkedList<T> elements;

	public QueueList() {
		elements = new LinkedList<T>();
	}

	public void enqueue(T elem) {
		elements.addLast(elem);
	}

	public boolean isEmpty() {
		return elements.isEmpty();
	}

	public T first() {
		if (isEmpty()) {
			throw new EmptyQueueException("first: empty queue");
		}
		return elements.getFirst();
	}

	public void dequeue() {
		if (isEmpty()) {
			throw new EmptyQueueException("dequeue: empty queue");
		}
		elements.removeFirst();
	}
	
	public String toString() {
		return "Queue"+elements;
	}
}
