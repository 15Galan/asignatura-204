package queue;

public class Example {
	public static void main(String [] args){
		Queue<Integer> queue = new QueuePLink<Integer>();
		queue.enqueue(5);
		queue.enqueue(9);
		queue.enqueue(2);
		queue.dequeue();
		queue.enqueue(4);
		queue.enqueue(5);
		queue.enqueue(8);
		System.out.println(queue);			
	}

}
