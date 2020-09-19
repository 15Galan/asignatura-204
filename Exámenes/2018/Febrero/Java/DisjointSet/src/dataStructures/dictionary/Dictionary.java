/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Interface for dictionaries
 */

package dataStructures.dictionary;

import dataStructures.tuple.Tuple2;

/**
 * Interface for dictionaries (finite maps) associating keys to values.
 * 
 * @param <K>
 *            Type of keys.
 * @param <V>
 *            Type of values.
 */
public interface Dictionary<K, V> {
    /**
     * Test for dictionary emptiness.
     * 
     * @return {@code true} if dictionary is empty (stores zero key/value
     *         associations), else {@code false}.
     */
    boolean isEmpty();

    /**
     * Retrieves number of key/value associations in dictionary.
     * 
     * @return Number of associations in dictionary.
     */
    int size();

    /**
     * Inserts a new key/value association in dictionary. If key was already
     * present in dictionary, old value is replaced by {@code v} (different
     * associations for same key are not supported).
     * 
     * @param k
     *            Key in association.
     * @param v
     *            Value associated to key.
     */
    void insert(K k, V v);

    /**
     * Retrieves value associated to key {@code k}. If key is not in dictionary,
     * {@code null} is returned.
     * 
     * @param k
     *            Key for which associated value is sought.
     * @return Value associated to key or {@code null} if key is not in
     *         dictionary.
     */
    V valueOf(K k);

    /**
     * Tests whether an association with key {@code k} is included in
     * dictionary.
     * 
     * @param k
     *            Key of association.
     * @return {@code true} if dictionary includes an association for key
     *         {@code k}, else {@code false}.
     */
    boolean isDefinedAt(K k);

    /**
     * Removes a key/value association from dictionary. If association with key
     * {@code k} is not in dictionary, dictionary is not modified (this is not
     * considered an error and thus no exception is thrown).
     * 
     * @param k
     *            Key of association to remove.
     */
    void delete(K k);

    /**
     * Retrieves an {@code Iterable} over all keys in dictionary. Note that
     * {@code remove} method is not supported in corresponding {@code iterator}.
     * Note also that dictionary structure or keys should not be modified during
     * iteration as iterator state may become inconsistent.
     * 
     * @see java.lang.Iterable
     * @return An {@code Iterable} over all keys in dictionary.
     */
    Iterable<K> keys();

    /**
     * Retrieves an {@code Iterable} over all values in dictionary. Note that
     * {@code remove} method is not supported in corresponding {@code iterator}.
     * Note also that dictionary structure or keys should not be modified during
     * iteration as iterator state may become inconsistent.
     * 
     * @see java.lang.Iterable
     * @return An {@code Iterable} over all keys in dictionary.
     */
    Iterable<V> values();

    /**
     * Retrieves an {@code Iterable} over all keys and values in dictionary.
     * Note that {@code remove} method is not supported in corresponding
     * {@code iterator}. Note also that dictionary structure or keys should not
     * be modified during iteration as iterator state may become inconsistent.
     * 
     * @see java.lang.Iterable
     * @return An {@code Iterable} over all keys in dictionary.
     */
    Iterable<Tuple2<K, V>> keysValues();
}
