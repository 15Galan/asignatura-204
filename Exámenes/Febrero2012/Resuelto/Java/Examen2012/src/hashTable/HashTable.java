package hashTable;


public interface HashTable<K, V> extends Iterable<K> {

	public boolean isEmpty();

	public int size();

	public void insert(K key, V value);

	public V search(K key);
	
	public boolean isElem(K key);

	public void delete(K key);

}