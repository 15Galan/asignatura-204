/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Dictionaries implemented as AVL trees
 */

package dataStructures.dictionary;

import dataStructures.searchTree.AVL;
import dataStructures.searchTree.SearchTree;
import dataStructures.tuple.Tuple2;

/**
 * Dictionaries (finite maps) associating different keys to values implemented
 * as AVL trees indexed by keys. Note that keys should define an order relation
 * ({@link java.lang.Comparable}).
 *
 * @param <K>
 *            Type of keys.
 * @param <V>
 *            Types of values.
 */
public class AVLDictionary<K extends Comparable<? super K>, V> implements Dictionary<K, V> {
    private SearchTree<K, V> tree;

    /**
     * Creates an empty dictionary.
     * <p>
     * Time complexity: O(1)
     */
    public AVLDictionary() {
        tree = new AVL<>();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(1)
     */
    public boolean isEmpty() {
        return tree.isEmpty();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(1)
     */
    public int size() {
        return tree.size();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(log n)
     */
    public void insert(K k, V v) {
        tree.insert(k, v);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(log n)
     */
    public void delete(K k) {
        tree.delete(k);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(log n)
     */
    public V valueOf(K k) {
        return tree.search(k);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Time complexity: O(log n)
     */
    public boolean isDefinedAt(K k) {
        return tree.isElem(k);
    }

    /**
     * {@inheritDoc}
     */
    public Iterable<K> keys() {
        return tree.inOrder();
    }

    /**
     * {@inheritDoc}
     */
    public Iterable<V> values() {
        return tree.values();
    }

    /**
     * {@inheritDoc}
     */
    public Iterable<Tuple2<K, V>> keysValues() {
        return tree.keysValues();
    }

    /**
     * Returns representation of dictionary object as a String.
     */
    public String toString() {
        String className = getClass().getSimpleName();
        String s = className + "(";
        if (!tree.isEmpty()) {
            for (Tuple2<K, V> t : tree.keysValues())
                s += t._1() + "->" + t._2() + ",";
            s = s.substring(0, s.length() - 1);
        }
        s += ")";
        return s;
    }
}
