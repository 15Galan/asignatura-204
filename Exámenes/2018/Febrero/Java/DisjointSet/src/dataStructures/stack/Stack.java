/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Interface for stacks
 */

package dataStructures.stack;

/**
 * Interface for Last In First Out data structures.
 *
 * @param <T>
 *            Type of elements in stack.
 */
public interface Stack<T> {
    /**
     * Test for stack emptiness.
     * 
     * @return {@code true} if stack is empty, else {@code false}.
     */
    boolean isEmpty();

    /**
     * Inserts new element on top of stack.
     * 
     * @param x
     *            the element to push.
     */
    void push(T x);

    /**
     * Retrieves (without removing) element on top of stack.
     * 
     * @throws EmptyStackException
     *             if stack is empty.
     * @return Element on top of stack.
     */
    T top();

    /**
     * Removes element on top of stack.
     * 
     * @throws EmptyStackException
     *             if stack is empty.
     */
    void pop();
}
