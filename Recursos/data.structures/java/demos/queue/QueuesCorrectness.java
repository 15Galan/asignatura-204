/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Checks that implementations for queues behave in the same way
 */

package demos.queue;

import dataStructures.queue.Queue;
import dataStructures.queue.ArrayQueue;
import dataStructures.queue.LinkedQueue;

public class QueuesCorrectness {

	public static void main(String[] args) {
		final int tests = 100;

		for (int s = 0; s < tests; s++) {
			Queue<Integer> lq = new LinkedQueue<>();
			Queue<Integer> aq = new ArrayQueue<>();

			RandomQueue.fill(lq, s, 100000);
			RandomQueue.fill(aq, s, 100000);

			while (!lq.isEmpty()) {
				int x = lq.first();
				int y = aq.first();
				lq.dequeue();
				aq.dequeue();
				if (x != y) {
					System.exit(1);
					System.out.println("Error");
				}
			}

			if (!aq.isEmpty()) {
				System.out.println("Error");
				System.exit(1);
			}

		}

  	System.out.println("CORRECT");
	}
}
