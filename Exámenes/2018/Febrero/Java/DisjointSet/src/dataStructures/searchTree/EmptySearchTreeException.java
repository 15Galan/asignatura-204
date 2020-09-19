/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Exceptions for priority queues
 */

package dataStructures.searchTree;

public class EmptySearchTreeException extends RuntimeException {

    private static final long serialVersionUID = 2553856243837331348L;

    public EmptySearchTreeException() {
        super();
    }

    public EmptySearchTreeException(String msg) {
        super(msg);
    }
}
