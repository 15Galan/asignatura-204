/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Interface for hash tables associating keys and values.
 */

package dataStructures.hashTable;

import dataStructures.tuple.Tuple2;


/**
 * Interface for hash tables whose entries are associations of keys to values.
 * Note that keys should define {@link Object#hashCode}
 * method properly.  
 * @param <K> Type of keys.
 * @param <V> Type of values.
 */
public interface HashTable<K, V> {

	/**
	 * Test for table emptiness.
	 * 
	 * @return {@code true} if table is empty (stores zero key/value associations), else {@code false}.
	 */
	boolean isEmpty();

	/**
	 * Retrieves number of key/value associations in table.
	 * @return Number of associations in table.
	 */
	int size();

	
	/**
	 * Inserts a new key/value association in table. If an association with
	 * same key was already present
	 * in table, old value is replaced by {@code v} (different associations for same key
	 * are not supported).
	 * @param k Key in association.
	 * @param v Value associated to key.
	 */
	void insert(K k, V v);


	/**
	 * Retrieves value associated to key {@code k}. If key is not
	 * in table, {@code null} is returned.
	 * @param k Key for which associated value is sought.
	 * @return Value associated to key or {@code null} if key is not in table. 
	 */
	V search(K k);
	

	/**
	 * Tests whether an association with key {@code k} is included in table.
	 * @param k Key of association.
	 * @return {@code true} if table includes an association for key {@code k}, else {@code false}. 
	 */
	boolean isElem(K k);

	/**
	 * Removes a key/value association from table. If association with key {@code k} is not
	 * in table, table is not modified (this is not considered an
	 * error and thus no exception is thrown).
	 * @param k Key of association to remove.
	 */
	void delete(K k);

	/** 
	 * Retrieves an {@code Iterable} over all keys in table.
	 * Note that {@code remove} method is not supported in corresponding {@code iterator}. 
	 * Note also that table structure or keys should not be modified during iteration as 
	 * iterator state may become inconsistent.
	 * @see Iterable
	 * @return An {@code Iterable} over all keys in table.
	 */
	Iterable<K> keys();
	
	/** 
	 * Retrieves an {@code Iterable} over all values in table.
	 * Note that {@code remove} method is not supported in corresponding {@code iterator}. 
	 * Note also that table structure or keys should not be modified during iteration as 
	 * iterator state may become inconsistent.
	 * @see Iterable
	 * @return An {@code Iterable} over all keys in table.
	 */
	Iterable<V> values();
	
	/** 
	 * Retrieves an {@code Iterable} over all keys and values in table.
	 * Note that {@code remove} method is not supported in corresponding {@code iterator}. 
	 * Note also that table structure or keys should not be modified during iteration as 
	 * iterator state may become inconsistent.
	 * @see Iterable
	 * @return An {@code Iterable} over all keys in table.
	 */
	Iterable<Tuple2<K,V>> keysValues();
}