package queue;

public class QueuePList<T extends Comparable<? super T>>  extends QueueList<T>  {
	
	public void enqueue(T elem) {
		int pos = 0;
		while ((pos < elements.size())
				&& (elem.compareTo(elements.get(pos)) > 0)) {
			pos++;
		}
		elements.add(pos, elem);
	}
}
