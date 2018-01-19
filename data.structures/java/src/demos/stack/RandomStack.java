/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Perform random operations on a stack
 */

package demos.stack;
import java.util.Random;

import dataStructures.stack.Stack;

public class RandomStack {
	public static void fill(Stack<Integer> s, int seed, int length) {
		Random rnd = new Random(seed);
		for (int i = 0; i < length; i++) {
			int r = rnd.nextInt(3);
			switch (r) {
			case 0:
			case 1:
				s.push(0);
				break;
			case 2:
				if (!s.isEmpty())
					s.pop();
			}
		}
	}
}
