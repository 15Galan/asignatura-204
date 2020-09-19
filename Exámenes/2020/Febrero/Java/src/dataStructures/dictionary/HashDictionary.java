/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Dictionaries implemented as Hash Tables
 */

package dataStructures.dictionary;

import dataStructures.hashTable.HashPrimes;
import dataStructures.hashTable.HashTable;
import dataStructures.hashTable.SeparateChainingHashTable;
import dataStructures.tuple.Tuple2;

/**
 * Dictionaries (finite maps) associating different keys to values
 * implemented as separate chaining hash tables. Note that keys should 
 * define {@link Object#hashCode} method properly.
 *
 * @param <K> Type of keys.
 * @param <V> Types of values.
 */
public class HashDictionary<K,V> implements Dictionary<K,V> {

	private HashTable<K,V> hashT;
	
	/**
	 * Creates an empty dictionary.
	 * <p>Time complexity: O(1)
	 */
	public HashDictionary() {
		hashT = new SeparateChainingHashTable<>(HashPrimes.primeGreaterThan(100),5);
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public void insert(K k, V v) {
		hashT.insert(k, v);
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public void delete(K k) {
		hashT.delete(k);
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public boolean isEmpty() {
		return hashT.isEmpty();
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public int size() {
		return hashT.size();
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public V valueOf(K k) {
		return hashT.search(k);
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public boolean isDefinedAt(K k) {
		return hashT.isElem(k);
	}
	
	/** 
	 * {@inheritDoc}
	 */
	public Iterable<K> keys() {
		return hashT.keys();
	}

	/** 
	 * {@inheritDoc}
	 */
	public Iterable<V> values() {
		return hashT.values();
	}

	/** 
	 * {@inheritDoc}
	 */
	public Iterable<Tuple2<K,V>> keysValues() {
		return hashT.keysValues();
	}

	/** 
	 * Returns representation of dictionary object as a String.
	 */
	public String toString() {
    String className = getClass().getSimpleName();
		String s = className+"(";
		if(!hashT.isEmpty()) {
			for(Tuple2<K,V> t : hashT.keysValues())
				s += t._1()+"->"+t._2()+",";
			s = s.substring(0, s.length()-1);
		}
		s += ")";
	  return s;
	}	
	
}
