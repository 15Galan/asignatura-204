/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Right case for union type
 */

package dataStructures.either;

import java.util.NoSuchElementException;

public class Right<A, B> implements Either<A, B> {
    private B right;

    /**
     * Creates a right value storing {@code x}
     * 
     * @param x
     *            Value to store in {@code Either object}.
     */
    public Right(B x) {
        right = x;
    }

    public boolean isLeft() {
        return false;
    }

    public boolean isRight() {
        return true;
    }

    public A left() {
        throw new NoSuchElementException("left on Right object");
    }

    public B right() {
        return right;
    }

    /**
     * Returns representation of {@code Either} object as a String.
     */
    public String toString() {
        String className = getClass().getName().substring(getClass().getPackage().getName().length() + 1);
        return className + "(" + right + ")";
    }
}
