/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Hash Table with separate chaining to resolve collisions
 */
 
package hashTable;

import java.util.Iterator;
import java.util.NoSuchElementException;

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
	
	public SeparateChainingHashTable(int numCells, double loadFactor) {
		table = (Node<K,V>[]) new Node[numCells];
		size = 0;
		maxLoadFactor = loadFactor;
	}	
	
	public boolean isEmpty() {
		return size==0;
	}
	
	public int size() {
		return size;
	}
	
	private int hash(K key) {
		return (key.hashCode() & 0x7fffffff) % table.length; 
	}
	
	private double loadFactor() {
		return (double)size / (double) table.length;
	}	
	
	private Node<K,V> searchNode(K key, int idx) {
		Node<K,V> current = table[idx];
		
		while((current != null) && (!current.key.equals(key)))
			current = current.next;
		
		return current;
	}
	
	public void insert(K key, V value) {
		if(loadFactor()>maxLoadFactor)
			rehashing();		
		
		int idx = hash(key);
		Node<K,V> node = searchNode(key, idx);
		if (node == null) {
			table[idx] = new Node<K,V>(key,value,table[idx]);
			size++;
		}
		else
			node.value = value;
	}
	
	public V search(K key) {
		int idx = hash(key);
		Node <K,V> node = searchNode(key, idx);
		return node==null ? null : node.value;
	}
	
	
	public boolean isElem(K key) {
		return search(key) != null;
	}		
	
	public void delete(K key) {
		int idx = hash(key);
		Node<K,V> prev = null, 
		          current = table[idx];

		while((current != null) && (!current.key.equals(key))) {
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
	
	
	// An iterator on keys in table
	private class TableIter implements Iterator<K> {
		private int idx;
		private Node<K,V> current;
		
		private void advance() {
			while((current == null) && (idx<table.length-1)) {
				idx++;			
				current = table[idx];
			}
		}
		
		public TableIter() {
			idx = 0;
			current = table[idx];
			
			// locate first elem
			advance();
		}
		
		public boolean hasNext() {
			return (current != null);
		}
		
		public K next() {
			if (!hasNext())
				throw new NoSuchElementException();
			K next = current.key;
			
			//advance for next invocation
			current = current.next;
			advance();
			
			return next;
		}
		
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}	
	
	public Iterator<K> iterator() {
		return new TableIter();
	}
	
}
