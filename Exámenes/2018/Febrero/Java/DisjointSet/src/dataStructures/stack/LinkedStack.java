/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Stack implementation using a linked list
 */

package dataStructures.stack;

/**
 * Stack implemented using a linked list.
 * 
 * @param <T>
 *            Type of elements in stack.
 */
public class LinkedStack<T> implements Stack<T> {
    static private class Node<E> {
        E elem;
        Node<E> next;

        public Node(E x, Node<E> node) {
            elem = x;
            next = node;
        }
    }

    private Node<T> top;

    /**
     * Creates an empty stack.
     * <p>
     * Time complexity: O(1)
     */
    public LinkedStack() {
        top = null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(1)
     */
    public boolean isEmpty() {
        return top == null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(1)
     * 
     * @throws EmptyStackException
     *             {@inheritDoc}
     */
    public T top() {
        if (isEmpty())
            throw new EmptyStackException("top on empty stack");
        else
            return top.elem;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(1)
     * 
     * @throws EmptyStackException
     *             {@inheritDoc}
     */
    public void pop() {
        if (isEmpty())
            throw new EmptyStackException("pop on empty stack");
        else
            top = top.next;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(1)
     */
    public void push(T x) {
        top = new Node<>(x, top);
    }

    /**
     * Returns representation of stack as a String.
     */
    @Override
    public String toString() {
        String className = getClass().getSimpleName();
        String s = className + "(";
        for (Node<T> node = top; node != null; node = node.next)
            s += node.elem + (node.next != null ? "," : "");
        s += ")";
        return s;
    }
}
