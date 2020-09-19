 /**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Perform random operations on a queue
 */

package demos.queue;

 import java.util.Random;

import dataStructures.queue.Queue;

public class RandomQueue {
	public static void fill(Queue<Integer> q, int seed, int length) {
		Random rnd = new Random(seed);
		for (int i = 0; i < length; i++) {
			int r = rnd.nextInt(3);
			switch (r) {
			case 0:
			case 1:
				q.enqueue(0);
				break;
			case 2:
				if (!q.isEmpty())
					q.dequeue();
			}
		}
	}
}
