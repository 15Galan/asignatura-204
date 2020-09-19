/**
 * @author Pepe Gallardo, Data Structures, Grado en Inform�tica. UMA.
 *
 * Hash Table with separate chaining to resolve collisions
 */
 
package dataStructures.hashTable;

import java.util.Iterator;
import java.util.NoSuchElementException;

import dataStructures.tuple.Tuple2;

/**
 * Hash tables whose entries are associations of different keys to values
 * implemented using separate chaining. Note that keys should 
 * define {@link java.lang.Object#hashCode} method properly.
 *
 * @param <K> Type of keys.
 * @param <V> Types of values.
 */
public class SeparateChainingHashTable<K,V> implements HashTable<K,V> {

	 static private class Node<K,V> {
		K key;
		V value;
		Node<K,V> next;
		
		public Node(K k, V v, Node<K,V> n) {
			key = k;
			value = v;
			next = n;
		}		
	}
	
	private Node<K,V> table[];
	private int size; // number of elements inserted in table
	private double maxLoadFactor;
	
	/**
	 * Creates an empty separate chaining hash table.
	 * <p>Time complexity: O(1)
   * @param numChains Number of separate chains (linked lists).
   * @param loadFactor Maximum load factor to tolerate. If exceeded, rehashing is performed automatically.	 
   */
	@SuppressWarnings("unchecked")
	public SeparateChainingHashTable(int numChains, double loadFactor) {
		table = (Node<K,V>[]) new Node[numChains];
		size = 0;
		maxLoadFactor = loadFactor;
	}	
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public boolean isEmpty() {
		return size==0;
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public int size() {
		return size;
	}
	
	private int hash(K k) {
		return (k.hashCode() & 0x7fffffff) % table.length; 
	}
	
	private double loadFactor() {
		return (double)size / (double) table.length;
	}	
	
	private Node<K,V> searchNode(K k, int idx) {
		Node<K,V> current = table[idx];
		
		while((current != null) && (!current.key.equals(k)))
			current = current.next;
		
		return current;
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public void insert(K k, V v) {
		if(loadFactor()>maxLoadFactor)
			rehashing();		
		
		int idx = hash(k);
		Node<K,V> node = searchNode(k, idx);
		if (node == null) {
			table[idx] = new Node<>(k,v,table[idx]);
			size++;
		}
		else
			node.value = v;
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public V search(K k) {
		int idx = hash(k);
		Node <K,V> node = searchNode(k, idx);
		return node==null ? null : node.value;
	}
	
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public boolean isElem(K k) {
		return search(k) != null;
	}		
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: near O(1)
	 */
	public void delete(K k) {
		int idx = hash(k);
		Node<K,V> prev = null, 
		          current = table[idx];

		while((current != null) && (!current.key.equals(k))) {
			prev = current;
			current = current.next;
		}
		
		if(current != null) { //found: delete it
			if(prev==null) // remove first node
				table[idx] = current.next;
			else
				prev.next = current.next;
			size--;
		}		
	}	
	
	
	@SuppressWarnings("unchecked")
	void rehashing() {
		// compute new table size
		int newCapacity = HashPrimes.primeDoubleThan(table.length);

		Node<K,V> oldTable[] = table;		
		table = (Node<K,V>[]) new Node[newCapacity];
		//System.out.printf("REHASH %d\n",newNumCells);		

		for(int i=0; i<oldTable.length; i++) {
			Node<K,V> current = oldTable[i];
			while(current != null) {
				Node<K,V> node = current;
				current = current.next; //for next iteration of while loop
				// insert node in new table
				int idx = hash(node.key);
				node.next = table[idx];
				table[idx] = node;
			}
		}	
		//System.out.printf("REHASH %d\n",newNumCells);		

	}
	
	
	// Almost an iterator on nodes in table
	private class NodesIter {
		private int idx;
		private Node<K,V> current;
		
		private void advance() {
			while((current == null) && (idx<table.length-1)) {
				idx++;			
				current = table[idx];
			}
		}
		
		public NodesIter() {
			idx = 0;
			current = table[idx];
			
			// locate first elem
			advance();
		}
		
		public boolean hasNext() {
			return (current != null);
		}
		
		public Node<K,V> nextNode() {
			if (!hasNext())
				throw new NoSuchElementException();
			Node<K,V> next = current;
			
			//advance for next invocation
			current = current.next;
			advance();
			
			return next;
		}
	}	

	
	// An iterator on values in table
	private class ValuesIter extends NodesIter implements Iterator<V> {
		public V next() {
			return super.nextNode().value;
		}
	}
	
	// An iterator on keys in table
	private class KeysIter extends NodesIter implements Iterator<K> {
		public K next() {
			return super.nextNode().key;
		}
	}
	
	// An iterator on keys/values in table
	private class KeysValuesIter extends NodesIter implements Iterator<Tuple2<K,V>> {
		public Tuple2<K,V> next() {
			Node<K,V> next = super.nextNode();
			return new Tuple2<K,V>(next.key, next.value);
		}
	}
	
	
	/** 
	 * {@inheritDoc}
	 */
	public Iterable<K> keys() {
		return new Iterable<K>(){
			public Iterator<K> iterator() {
				return new KeysIter();
			}
		};
	}
	
	/** 
	 * {@inheritDoc}
	 */
	public Iterable<V> values() {
		return new Iterable<V>(){
			public Iterator<V> iterator() {
				return new ValuesIter();
			}
		};
	}
	
	/** 
	 * {@inheritDoc}
	 */
	public Iterable<Tuple2<K,V>> keysValues() {
		return new Iterable<Tuple2<K,V>>(){
			public Iterator<Tuple2<K,V>> iterator() {
				return new KeysValuesIter();
			}
		};
	}
	
	/** 
	 * Returns representation of hash table as a String.
	 */
 	public String toString() {
		String className = getClass().getSimpleName();
		String s = className+"(";
		if(!isEmpty()) {
			for(Tuple2<K,V> t : keysValues())
				s += t._1()+"->"+t._2()+",";
			s = s.substring(0, s.length()-1);
		}
		s += ")";
		return s;
	}
	
}
