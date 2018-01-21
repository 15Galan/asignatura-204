package set;

public interface Set<T> extends Iterable<T> {
	boolean isEmpty();
	int size();
	void insert(T elem);
	boolean isElem(T elem);
	void delete(T elem);
}
