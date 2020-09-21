/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Tests for First Fit algorithms for Bin Packing problem
 */

import dataStructures.list.List;

public class BinPackingDemo {
    public static void main(String[] args) {
        main1(args);
        System.out.println();
        main2(args);
    }

    public static void test(int initialCapacity, int[] weights) {
        System.out.printf("Solving a problem instance with %d as initial capacity and these weights", initialCapacity);
        for(int weight : weights)
            System.out.printf(" %d",weight);
        System.out.println(".");
        AVL avl = new AVL();
        avl.addAll(initialCapacity, weights);
        List<Bin> bins = avl.toList();
        System.out.println("Solution is: ");
        System.out.println(bins);
    }

    public static void main1(String[] args) {
        test(10, new int[]{ 9, 9, 9, 9, 9, 9, 9, 9, 9 });
    }

    public static void main2(String[] args) {
        test(10, new int[]{ 5, 7, 5, 2, 4, 2, 5, 1, 6 });
    }
}