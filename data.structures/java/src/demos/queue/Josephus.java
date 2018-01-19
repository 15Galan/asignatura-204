/**
 * @author Pablo López, Data Structures, Grado en Informática. UMA.
 *
 * Simulation of Flavius Josephus' problem using a Queue.
 */

package demos.queue;

import dataStructures.queue.*;
import java.util.LinkedList;

public class Josephus {
	public static void main(String[] args) {

		final int NUM_PRISONERS = Integer.parseInt(args[0]);
		final int PRISONERS_TO_SKIP = Integer.parseInt(args[1]);

		Queue<Integer> prisoners = new ArrayQueue<>();
		for (int i = 0; i < NUM_PRISONERS; i++) {
			prisoners.enqueue(i);
		}

		LinkedList<Integer> killed = new LinkedList<>();
		while (killed.size() != NUM_PRISONERS - 1) {
			// skip prisoners
			for (int i = 0; i < PRISONERS_TO_SKIP; i++) {
				Integer p = prisoners.first();
				prisoners.dequeue();
				prisoners.enqueue(p);
			}
			// kill the first one
			Integer toBeKilled = prisoners.first();
			prisoners.dequeue();
			killed.addLast(toBeKilled);
		}

		System.out.println("prisoners killed = " + killed);
		System.out.println("prisoner saved = " + prisoners.first());
	}
}
