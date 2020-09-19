package list;

public interface List<T> extends Iterable<T>{
	T get(int i);

	void set(int i, T el);

	void add(int i, T el);

	void remove(int i);

	boolean isEmpty();
	
	int size();
}
