/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Experimental comparison for list implementations
 */

package demos.list;

import dataStructures.list.List;
import dataStructures.list.ArrayList;
import dataStructures.list.LinkedList;

public class ListsPerformance {
		
	public enum Implementation { WithArray, Linked};
	
	public static double test(Implementation impl, int seed, int length) {
		long t0 = System.currentTimeMillis();
		List<Integer> list = impl==Implementation.WithArray ? new ArrayList<>()
				                                            : new LinkedList<>();
		RandomList.fill(list, seed, length);
		long t1 = System.currentTimeMillis();
		return (t1-t0) / 10e3; //execution time in seconds
	}	
	
	static double avgTime(Implementation impl, int tests) {
		double t = 0.0;
		
		for(int s=0; s<tests; s++) 
			t += test(impl, s, 100000);

		return t/tests;	
	}
	
	public static void main(String[] args) {
		
		final int tests = 10;
		
		double t1 = avgTime(Implementation.Linked, tests);
		double t2 = avgTime(Implementation.WithArray, tests);
		
		System.out.printf("Average execution time for LinkedList is %f seconds\n", t1);
		System.out.printf("Average execution time for ArrayList is %f seconds\n", t2);
		System.out.printf("ArrayList is %.2f times faster", t1/t2);		
		
	}	
}
