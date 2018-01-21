package queue;
import stack.*;
public class Queue2Stack<T> implements Queue<T>{
	private Stack<T> stackI, stackD;
	
	public Queue2Stack() {
		stackI = new StackLink<T>();
		stackD = new StackLink<T>();
	}
	
	private void mkValid() {
		if (stackI.isEmpty()) {
			while (!stackD.isEmpty()) {
				stackI.push(stackD.top());
				stackD.pop();
			}
		}
	}
	
	public boolean isEmpty() {
		return stackI.isEmpty();
	}
	
	public void enqueue(T elem) {
		stackD.push(elem);
		mkValid();
	}
	
	public void dequeue() {
		if (isEmpty()) {
			throw new EmptyQueueException("dequeue: Queue empty");
		}
		stackI.pop();
		mkValid();
	}
	
	public T first() {
		if (isEmpty()) {
			throw new EmptyQueueException("dequeue: Queue empty");
		}
		return stackI.top();		
	}
	
	public String toString() {
		Stack<T> sI = new StackLink<T>(stackI);
		Stack<T> sD = new StackLink<T>(stackD);
		String salida = "Queue ( ";
		while (!sI.isEmpty()) {
			salida += sI.top() +" ";
			sI.pop();
		}
		while (!sD.isEmpty()) {
			sI.push(sD.top());
			sD.pop();
		}
		while (!sI.isEmpty()) {
			salida += sI.top() +" ";
			sI.pop();
		}
		return salida + " )";
	}
}
