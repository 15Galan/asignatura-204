package queue;

public class QueuePLink<T extends Comparable<? super T>>  extends QueueLink<T> {
		public void enqueue(T elem) {
			Node<T> aux = first;
			Node<T> prev = null;
			
			while ((aux != null)
					&& (elem.compareTo(aux.data) > 0)) {
				prev = aux;
				aux = aux.next;
			}
			if (prev == null) {
				first = new Node<T>(elem, first);
				if (last == null)
					last = first;
			} else {
				prev.next = new Node<T>(elem,prev.next);
		}
	}

}
