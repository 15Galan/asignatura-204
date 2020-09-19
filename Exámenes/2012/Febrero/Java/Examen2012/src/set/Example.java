package set;

import java.util.Random;

public class Example {
	static Random alea = new Random();

	static public void main(String[] args) {
		Set<Integer> set = new HashSet<Integer>();
		for (int i = 0; i < 100; i++) {
			set.insert(alea.nextInt(100));
		}
		for (int i = 0; i < 100; i++) {
			if (set.isElem(i)) {
				System.out.println(i);
				set.delete(i);
			}
		}
		System.out.println(set.isEmpty());
	}
}
