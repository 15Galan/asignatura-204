package tree;

import java.util.Iterator;

public interface SearchTree<K extends Comparable<? super K>, V> extends Iterable<K> {

	public  boolean isEmpty();

	public  int size();
	
	public int height();

	public  void insert(K k, V v);

	public  V search(K key);

	public  boolean isElem(K key);

	public  void delete(K key);

	public  Iterator<K> inOrder();

	public  Iterator<K> postOrder();

	public  Iterator<K> preOrder();

}