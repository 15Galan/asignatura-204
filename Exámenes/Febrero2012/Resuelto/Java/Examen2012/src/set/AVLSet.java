/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Sets implemented as AVL trees
 */

package set;

import java.util.Iterator;

import tree.AVL;
import tree.SearchTree;

public class AVLSet<T extends Comparable<? super T>> implements Set<T>{
	
	private static class Nothing{};
	private Nothing nothing = new Nothing();
	
	private SearchTree<T,Nothing> tree;	
	
	public AVLSet() {
		tree = new AVL<T,Nothing>();
	}
	
	public void delete(T elem) {
		tree.delete(elem);		
	}

	public void insert(T elem) {
		tree.insert(elem,nothing);
	}

	public boolean isElem(T elem) {
		return tree.isElem(elem);
	}

	public int size() {
		return tree.size();
	}

	public boolean isEmpty() {
		return tree.isEmpty();
	}

	public Iterator<T> iterator() {
		return tree.iterator();
	}
	
	public String toString() {
		String s = "AVLSet(";
		Iterator<T> it = this.iterator();
		while(it.hasNext()) 
			s += it.next() + (it.hasNext() ? "," : "");
		s += ")";
		return s;
	}
}
