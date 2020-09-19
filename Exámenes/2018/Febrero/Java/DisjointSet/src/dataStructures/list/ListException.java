/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Exceptions for lists
 */

package dataStructures.list;

public class ListException extends RuntimeException {

    private static final long serialVersionUID = -17371304231172484L;

    public ListException() {
        super();
    }

    public ListException(String msg) {
        super(msg);
    }
}
