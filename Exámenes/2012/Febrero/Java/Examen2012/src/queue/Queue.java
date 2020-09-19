package queue;

public interface Queue<T> {
	void enqueue(T elem);

	T first();

	void dequeue();

	boolean isEmpty();
}
