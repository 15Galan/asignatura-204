/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Left case for union type
 */

package dataStructures.either;

import java.util.NoSuchElementException;

public class Left<A, B> implements Either<A, B> {
    private A left;

    /**
     * Creates a left value storing {@code x}
     * 
     * @param x
     *            Value to store in {@code Either object}.
     */
    public Left(A x) {
        left = x;
    }

    public boolean isLeft() {
        return true;
    }

    public boolean isRight() {
        return false;
    }

    public A left() {
        return left;
    }

    public B right() {
        throw new NoSuchElementException("right on Left object");
    }

    /**
     * Returns representation of {@code Either} object as a String.
     */
    public String toString() {
        String className = getClass().getName().substring(getClass().getPackage().getName().length() + 1);
        return className + "(" + left + ")";
    }
}
