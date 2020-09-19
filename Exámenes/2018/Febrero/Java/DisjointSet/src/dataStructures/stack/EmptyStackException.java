/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Exceptions for stacks
 */

package dataStructures.stack;

public class EmptyStackException extends RuntimeException {
    private static final long serialVersionUID = -6980591028488182709L;

    public EmptyStackException() {
        super();
    }

    public EmptyStackException(String msg) {
        super(msg);
    }
}
