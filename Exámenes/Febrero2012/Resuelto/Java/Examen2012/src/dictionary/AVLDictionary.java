/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Dictionaries implemented as AVL trees
 */

package dictionary;
import java.util.Iterator;

import tree.AVL;
import tree.SearchTree;


public class AVLDictionary<K extends Comparable<? super K>,V> implements Dictionary<K,V> {
	private SearchTree<K,V> tree;
	
	public AVLDictionary() {
		tree = new AVL<K,V>();
	}
	
	public boolean isEmpty() {
		return tree.isEmpty();
	}

	public int size() {
		return tree.size();
	}
	
	public void insert(K k, V v) {
		tree.insert(k, v);
	}
	
	public void delete(K k) {
		tree.delete(k);
	}

	public V valueOf(K k) {
		return tree.search(k);
	}	
	
	public Iterable<K> keys() {
		return new Iterable<K>(){
			public Iterator<K> iterator() {
				return tree.iterator();
			}
		};
	}

	public Iterator<K> iterator() {
		return tree.iterator();
	}
	
	public String toString() {
		String s = "AVLDictionary(";
		for(K k : tree)
			s += k+"->"+tree.search(k)+",";
		s = s.substring(0, s.length()-1);
		s += ")";
	  return s;
	}
}
