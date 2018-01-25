/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Experimental comparison for queue implementations
 */

package demos.queue;

import dataStructures.queue.Queue;
import dataStructures.queue.ArrayQueue;
import dataStructures.queue.LinkedQueue;

public class QueuesPerformance {
	
	public enum Implementation { WithArray, Linked};
	
	public static double test(Implementation impl, int seed, int length) {
		long t0 = System.currentTimeMillis();
		Queue<Integer> queue = impl==Implementation.WithArray ? new ArrayQueue<>()
				                                              : new LinkedQueue<>(); 
		RandomQueue.fill(queue, seed, length);
		long t1 = System.currentTimeMillis();
		return (t1-t0) / 10e3; //execution time in seconds
	}	
	
	static double avgTime(Implementation impl, int tests) {
		double t = 0.0;
		
		for(int s=0; s<tests; s++) 
			t += test(impl, s, 10000000);

		return t/tests;	
	}
	
	public static void main(String[] args) {
		
		final int tests = 10;
		
		double t1 = avgTime(Implementation.Linked, tests);
		double t2 = avgTime(Implementation.WithArray, tests);
		
		System.out.printf("Average execution time for LinkedQueue is %f seconds\n", t1);
		System.out.printf("Average execution time for ArrayQueue is %f seconds\n", t2);
		System.out.printf("ArrayQueue is %.2f times faster", t1/t2);		
		
	}	
}