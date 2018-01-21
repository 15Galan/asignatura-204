package dictionary;

import java.util.Iterator;

public interface Dictionary<K, V> extends Iterable<K> {
	void insert(K k, V v);
	V valueOf(K k);
	Iterable<K> keys();
	int size();
	boolean isEmpty();
	void delete(K k);
}
