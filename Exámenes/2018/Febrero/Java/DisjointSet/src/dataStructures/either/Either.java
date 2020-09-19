/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Union type
 */

package dataStructures.either;

/**
 * Union type. Can store either a value of type {@code A} (object is a left
 * value) or a value of type {@code B} (object is a right value), but does not
 * store both values simultaneously.
 *
 * @param <A>
 *            Type of left values.
 * @param <B>
 *            Type of right values.
 */
public interface Either<A, B> {
    /**
     * Tests whether this {@code Either} object is a left value.
     * 
     * @return {@code true} if object is left value, else {@code false}.
     */
    boolean isLeft();

    /**
     * Tests whether this {@code Either} object is a right value.
     * 
     * @return {@code true} if object is right value, else {@code false}.
     */
    boolean isRight();

    /**
     * Retrieves left value stored in {@code Either} object.
     * 
     * @return left value stored in {@code Either} object.
     * @throws <code>NoSuchElementException</code>
     *             if object is not a left value.
     */
    A left();

    /**
     * Retrieves right value stored in {@code Either} object.
     * 
     * @return right value stored in {@code Either} object.
     * @throws <code>NoSuchElementException</code>
     *             if object is not a right value.
     */
    B right();
}
