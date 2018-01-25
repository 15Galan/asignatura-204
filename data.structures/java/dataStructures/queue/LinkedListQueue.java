/**
 * @author Paco Gutiérrez, Data Structures, Grado en Informática. UMA.
 *
 * Queue implemented using java´s linked lists.
 */

package dataStructures.queue;

import java.util.LinkedList;
import java.util.Iterator;

public class LinkedListQueue<T> implements Queue<T> {
    protected LinkedList<T> elements;

    /**
     * Creates an empty queue.
     * <p>Time complexity: O(1)
     */
    public LinkedListQueue() {
        elements = new LinkedList<>();
    }

    /**
     * {@inheritDoc}
     * <p>Time complexity: O(1)
     */
    public void enqueue(T elem) {
        elements.addLast(elem);
    }

    /**
     * {@inheritDoc}
     * <p>Time complexity: O(1)
     */
    public boolean isEmpty() {
        return elements.isEmpty();
    }

    /**
     * {@inheritDoc}
     * <p>Time complexity: O(1)
     *
     * @throws EmptyQueueException {@inheritDoc}
     */
    public T first() {
        if (isEmpty()) {
            throw new EmptyQueueException("pop: empty queue");
        }
        return elements.getFirst();
    }

    /**
     * {@inheritDoc}
     * <p>Time complexity: O(1)
     *
     * @throws EmptyQueueException {@inheritDoc}
     */
    public void dequeue() {
        if (isEmpty()) {
            throw new EmptyQueueException("pop: empty stack");
        }
        elements.removeFirst();
    }

    /**
     * Returns representation of queue as a String.
     */
    public String toString() {
        String className = getClass().getName().substring(getClass().getPackage().getName().length() + 1);
        String text = className + "(";
        Iterator<T> it = elements.iterator();
        while (it.hasNext()) {
            text += it.next() + (it.hasNext() ? "," : "");
        }
        return text + ")";
    }
}
