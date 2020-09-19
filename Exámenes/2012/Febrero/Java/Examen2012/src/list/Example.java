package list;

import java.util.Random;

public class Example {
	static Random alea = new Random();

	static public void main(String[] args) {
		List<Integer> list = new ListLink<Integer>();
		for (int i = 0; i < 10; i++) {
			list.add(i,alea.nextInt(100));
		}
		list.add(1, 101);
		list.set(2, 102);	
		System.out.println(list);
		for (int i = 0; i < 11; i++) {
			System.out.println(list.get(i));
		}
		list.remove(1);
		for (int i = 0; i < 10; i++) {
			System.out.println(list.get(i));
		}
		System.out.println(list.isEmpty());
	}

}
