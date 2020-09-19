/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Interface for search trees associating keys and values.
 */

package dataStructures.searchTree;

import dataStructures.tuple.Tuple2;

import java.util.function.UnaryOperator;

/**
 * Interface for search trees. Each node stores a key and a value. Elements are
 * sorted in tree according to their keys, providing thus fast search on these
 * keys. Note that keys should define an order relation
 * {@link java.lang.Comparable}.
 * 
 * @param <K>
 *            Type of keys.
 * @param <V>
 *            Type of values.
 */
public interface SearchTree<K extends Comparable<? super K>, V> {

    /**
     * Test for tree emptiness.
     * 
     * @return {@code true} if tree is empty (has zero key/value associations),
     *         else {@code false}.
     */
    boolean isEmpty();

    /**
     * Retrieves number of key/value associations in tree.
     * 
     * @return Number of associations in tree.
     */
    int size();

    /**
     * Retrieves height of tree.
     * 
     * @return Height of tree.
     */
    int height();

    /**
     * Inserts a new key/value association in tree. If key was already present
     * in tree, old value is replaced by {@code v} (different associations for
     * same key are not supported).
     * 
     * @param k
     *            Key in association.
     * @param v
     *            Value associated to key.
     */
    void insert(K k, V v);

    /**
     * Retrieves value associated to key {@code k}. If key is not in tree,
     * {@code null} is returned.
     * 
     * @param k
     *            Key for which associated value is sought.
     * @return Value associated to key or {@code null} if key is not in tree.
     */
    V search(K k);

    /**
     * Tests whether an association with key {@code k} is included in tree.
     * 
     * @param k
     *            Key of association.
     * @return {@code true} if tree includes an association for key {@code k},
     *         else {@code false}.
     */
    boolean isElem(K k);

    /**
     * Removes a key/value association from tree. If association with key
     * {@code k} is not in tree, tree is not modified (this is not considered an
     * error and thus no exception is thrown).
     * 
     * @param k
     *            Key of association to remove.
     */
    void delete(K k);

    /**
     * Returns value of node with minimum key in tree.
     * 
     * @return value for minimum key.
     */
    V minim();

    /**
     * Returns value of node with maximum key in tree.
     * 
     * @return value for minimum key.
     */
    V maxim();

    /**
     * Removes node with minimum key from tree.
     */
    void deleteMinim();

    /**
     * Removes node with maximum key from tree.
     */
    void deleteMaxim();

    /**
     * Retrieves an {@code Iterable} over all keys in tree. Keys are visited in
     * in-order. Note that {@code remove} method is not supported in
     * corresponding {@code iterator}. Note also that tree structure or keys
     * should not be modified during iteration as iterator state may become
     * inconsistent.
     * 
     * @see java.lang.Iterable
     * @return An {@code Iterable} over all keys in tree.
     */
    Iterable<K> inOrder();

    /**
     * Retrieves an {@code Iterable} over all keys in tree. Keys are visited in
     * post-order. Note that {@code remove} method is not supported in
     * corresponding {@code iterator}. Note also that tree structure or keys
     * should not be modified during iteration as iterator state may become
     * inconsistent.
     * 
     * @see java.lang.Iterable
     * @return An {@code Iterable} over all keys in tree.
     */
    Iterable<K> postOrder();

    /**
     * Retrieves an {@code Iterable} over all keys in tree. Keys are visited in
     * pre-order. Note that {@code remove} method is not supported in
     * corresponding {@code iterator}. Note also that tree structure or keys
     * should not be modified during iteration as iterator state may become
     * inconsistent.
     * 
     * @see java.lang.Iterable
     * @return An {@code Iterable} over all keys in tree.
     */
    Iterable<K> preOrder();

    /**
     * Retrieves an {@code Iterable} over all values in tree. Values are visited
     * following an in-order traversal of corresponding keys. Note that
     * {@code remove} method is not supported in corresponding {@code iterator}.
     * Note also that tree structure or keys should not be modified during
     * iteration as iterator state may become inconsistent.
     * 
     * @see java.lang.Iterable
     * @return An {@code Iterable} over all keys in tree.
     */
    Iterable<V> values();

    /**
     * Retrieves an {@code Iterable} over all keys/values associations in tree.
     * Associations are visited following an in-order traversal of their keys.
     * Note that {@code remove} method is not supported in corresponding
     * {@code iterator}. Note also that tree structure or keys should not be
     * modified during iteration as iterator state may become inconsistent.
     * 
     * @see java.lang.Iterable
     * @return An {@code Iterable} over all keys in tree.
     */
    Iterable<Tuple2<K, V>> keysValues();

    /**
     * Inserts a new key/value association in tree. If key was already present
     * in tree, old value is replaced by {@code f(old value)} (different
     * associations for same key are not supported).
     * 
     * @param f
     *            function to apply to current value if key is already present
     *            in tree.
     * @param k
     *            Key in association.
     * @param v
     *            Value associated to key.
     */
    void updateOrInsert(UnaryOperator<V> f, K k, V v);

}