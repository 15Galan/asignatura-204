/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Sets implemented as Hash Tables
 */

package dataStructures.set;

import dataStructures.hashTable.HashPrimes;
import dataStructures.hashTable.HashTable;
import dataStructures.hashTable.SeparateChainingHashTable;

import java.util.Iterator;


/**
 * Sets implemented using separate chaining hash tables.
 * Note that elements should 
 * define {@link Object#hashCode} method properly.
 * @param <T> Type of elements in set.
 */
public class HashSet<T> implements Set<T> {
	private static class Nothing{};
	private Nothing nothing = new Nothing();

	private HashTable<T,Nothing> hashT;
	
	/**
	 * Creates a new empty set.
	 * <p>Time complexity: O(1)
	 */
  public HashSet() {
		hashT = new SeparateChainingHashTable<>(HashPrimes.primeGreaterThan(100),5);
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public void delete(T elem) {
		hashT.delete(elem);
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public void insert(T elem) {
		hashT.insert(elem, nothing);
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public boolean isElem(T elem) {
		return hashT.isElem(elem);
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
	 * Iterator over elements in set.
	 * Note that {@code remove} method is not supported. Note also 
	 * that set should not be modified during iteration as 
	 * iterator state may become inconsistent.
	 * @see Iterable#iterator()
	 */
	public Iterator<T> iterator() {
		return hashT.keys().iterator();
	}

	/** 
	 * Returns representation of set as a String.
	 */
	public String toString() {
    	String className = getClass().getSimpleName();
		String s = className+"(";
		Iterator<T> it = this.iterator();
		while(it.hasNext()) 
			s += it.next() + (it.hasNext() ? "," : "");
		s += ")";
		return s;
	}	
	
}
