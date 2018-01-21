package stack;

public interface Stack<T> {
	void push(T elem);

	T top();

	void pop();

	boolean isEmpty();
}
