package dataStructures.tuple;

/**
 * Tuples with two components.
 *
 * @param <A>
 *            Type of first component.
 * @param <B>
 *            Type of second component.
 */
public class Tuple2<A, B> {
    private A elem1;
    private B elem2;

    /**
     * Creates a tuple with two components.
     * 
     * @param x
     *            First component.
     * @param y
     *            Second component.
     */
    public Tuple2(A x, B y) {
        elem1 = x;
        elem2 = y;
    }

    /**
     * Retrieves first component of tuple.
     * 
     * @return First component of tuple.
     */
    public A _1() {
        return elem1;
    }

    /**
     * Retrieves second component of tuple.
     * 
     * @return Second component of tuple.
     */
    public B _2() {
        return elem2;
    }

    /**
     * Returns representation of tuple as a String.
     */
    @Override
    public String toString() {
        String className = getClass().getSimpleName();
        return className + "(" + elem1 + "," + elem2 + ")";
    }
}
