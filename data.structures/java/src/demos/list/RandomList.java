/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Perform random operations on a list
 */

package demos.list;

import java.util.Random;

import dataStructures.list.List;

public class RandomList {
	public static void fill(List<Integer> l, int seed, int length) {
		Random rnd = new Random(seed);
		for (int i = 0; i < length; i++) {
			int r = rnd.nextInt(4);
			switch (r) {
			case 0:
			case 1: {
				int p = rnd.nextInt(l.size()+1);
				l.insert(p,0);
				break;
			}	

			case 2:
				if (!l.isEmpty()) {
					int p = rnd.nextInt(l.size());
					l.remove(p);
				}
				break;

			case 3: {
				if (!l.isEmpty()) {
					int p = rnd.nextInt(l.size());
					int x = l.get(p);
				}
				break;
			}	
			}
		}
	}
}
