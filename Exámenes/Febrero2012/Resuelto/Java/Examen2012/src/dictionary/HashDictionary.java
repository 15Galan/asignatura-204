/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Dictionaries implemented as Hash Tables
 */

package dictionary;

import java.util.Iterator;

import hashTable.HashPrimes;
import hashTable.HashTable;
import hashTable.SeparateChainingHashTable;

public class HashDictionary<K,V> implements Dictionary<K,V> {

	private HashTable<K,V> hashT;
	
	public HashDictionary() {
		hashT = new SeparateChainingHashTable<K,V>(HashPrimes.primeGreaterThan(100),5);
	}
	
	public void insert(K k, V v) {
		hashT.insert(k, v);
	}

	public void delete(K k) {
		hashT.delete(k);
	}

	public boolean isEmpty() {
		return hashT.isEmpty();
	}
	
	public Iterable<K> keys() {
		return new Iterable<K>(){
			public Iterator<K> iterator() {
				return hashT.iterator();
			}
		};
	}	

	public int size() {
		return hashT.size();
	}

	public V valueOf(K k) {
		return hashT.search(k);
	}

	public Iterator<K> iterator() {
		return hashT.iterator();
	}

	public String toString() {
		String s = "HashDictionary(";
		for(K k : hashT)
			s += k+"->"+hashT.search(k)+",";
		s = s.substring(0, s.length()-1);
		s += ")";
	  return s;
	}	
	
}
