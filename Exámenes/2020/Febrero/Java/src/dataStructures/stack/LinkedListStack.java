/**
 * @author Paco Guti�rrez, Data Structures, Grado en Inform�tica. UMA.
 *
 * Stack implemented using java�s linked lists.
 */

package dataStructures.stack;

import java.util.Iterator;
import java.util.LinkedList;

public class LinkedListStack<T> implements Stack<T> {
    protected LinkedList<T> elements;

    /**
     * Creates an empty stack.
     * <p>Time complexity: O(1)
     */
    public LinkedListStack() {
        elements = new LinkedList<>();
    }

    /**
     * {@inheritDoc}
     * <p>Time complexity: O(1)
     */
    public void push(T elem) {
        elements.addFirst(elem);
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
     * @throws EmptyStackException {@inheritDoc}
     */
    public T top() {
        if (isEmpty()) {
            throw new EmptyStackException("pop: empty stack");
        }
        return elements.getFirst();
    }

    /**
     * {@inheritDoc}
     * <p>Time complexity: O(1)
     *
     * @throws EmptyStackException {@inheritDoc}
     */
    public void pop() {
        if (isEmpty()) {
            throw new EmptyStackException("pop: empty stack");
        }
        elements.removeFirst();
    }

    /**
     * Returns representation of stack as a String.
     */
    public String toString() {
        String className = getClass().getSimpleName();
        String text = className + "(";
        Iterator<T> it = elements.iterator();
        while (it.hasNext()) {
            text += it.next() + (it.hasNext() ? "," : "");
        }
        return text + ")";
    }
}
