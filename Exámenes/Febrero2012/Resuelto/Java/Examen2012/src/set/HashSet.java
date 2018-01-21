/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Sets implemented as Hash Tables
 */

package set;

import java.util.Iterator;

import hashTable.HashPrimes;
import hashTable.HashTable;
import hashTable.SeparateChainingHashTable;


public class HashSet<T> implements Set<T> {
	private static class Nothing{};
	private Nothing nothing = new Nothing();

	private HashTable<T,Nothing> hashT;
	
	public HashSet() {
		hashT = new SeparateChainingHashTable<T,Nothing>(HashPrimes.primeGreaterThan(100),5);
	}
	
	public void delete(T elem) {
		hashT.delete(elem);
	}

	public void insert(T elem) {
		hashT.insert(elem, nothing);
	}

	public boolean isElem(T elem) {
		return hashT.isElem(elem);
	}

	public boolean isEmpty() {
		return hashT.isEmpty();
	}

	public int size() {
		return hashT.size();
	}

	public Iterator<T> iterator() {
		return hashT.iterator();
	}

	public String toString() {
		String s = "HashSet(";
		Iterator<T> it = this.iterator();
		while(it.hasNext()) 
			s += it.next() + (it.hasNext() ? "," : "");
		s += ")";
		return s;
	}	
	
}
