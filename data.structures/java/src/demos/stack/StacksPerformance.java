/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Experimental comparison for stack implementations
 */

package demos.stack;

import dataStructures.stack.Stack;
import dataStructures.stack.ArrayStack;
import dataStructures.stack.LinkedStack;

public class StacksPerformance {
		
	public enum Implementation { WithArray, Linked};
	
	public static double test(Implementation impl, int seed, int length) {
		long t0 = System.currentTimeMillis();
		Stack<Integer> stack = impl==Implementation.WithArray ? new ArrayStack<>()
				                                              : new LinkedStack<>(); 
		RandomStack.fill(stack, seed, length);
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
		
		System.out.printf("Average execution time for LinkedStack is %f seconds\n", t1);
		System.out.printf("Average execution time for ArrayStack is %f seconds\n", t2);
		System.out.printf("ArrayStack is %.2f times faster", t1/t2);		
		
	}	
}
