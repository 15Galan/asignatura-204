/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Simple Stack demo
 */

package demos.stack;

import dataStructures.stack.Stack;
import dataStructures.stack.ArrayStack;
import dataStructures.stack.LinkedStack;

public class StackDemo {
	public static void main(String[] args) {
		Stack<Integer> s1 = new LinkedStack<>();
		s1.push(1);
		s1.push(2);
		s1.push(3);
		int x = s1.top();
		s1.pop();
		
		Stack<Integer> s2 = new ArrayStack<>();
		s2.push(1);
		s2.push(2);
		s2.push(3);
		
		System.out.println(s1);
		System.out.println(s2);
	}
}
