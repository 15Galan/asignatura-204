package dataStructures.vector;

import java.util.Iterator;

import dataStructures.vector.SparseVector;
import dataStructures.vector.SparseVector.*;

public class SparseVectorTesting {

    public static void main(String[] args) {

        final int N = 3;
        final int VECTOR_SIZE = (int) Math.pow(2, N);
        char INITIAL_ELEM = 'a';
        SparseVector<Character> vector1;

        /*************************************************************************/

        // Complete SparseVector constructor and size method

        System.out.println("testing SparseVector constructor and size");

        vector1 = new SparseVector<>(N, INITIAL_ELEM);
        check(vector1.size() == VECTOR_SIZE, "vector has wrong size");
        check(new SparseVector<>(0, 1).size() == 1, "vector has wrong size");

        System.out.println("DONE!\n");

        /*************************************************************************/

        // Complete get methods of classes SparseVector and Unif

        System.out.println("testing get methods of SparseVector and Unif");

        vector1 = new SparseVector<>(N, INITIAL_ELEM);
        for (int i = 0; i < vector1.size(); i++) {
            check(INITIAL_ELEM == vector1.get(i), "get returns wrong value");
        }

        System.out.println("DONE!\n");

        /*************************************************************************/

        // Complete constructor and get method of class Node

        System.out.println("testing constructor and get method of Node");

        // build vector2 and get all its elements
        {
            Tree<Character> vector2 = new Node<>(new Unif<>('a'), new Unif<>('b'));
            int vector2Size = 8;
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < vector2Size; i++) {
                result.append(vector2.get(vector2Size, i));
            }
            check("aaaabbbb".equals(result.toString()), "get returns wrong value");
        }

        // build vector3 and get all its elements
        {
            Tree<Character> vector3 = new Node<>(
                    new Node<>(new Node<>(new Unif<>('a'), new Unif<>('b')),
                            new Node<>(new Unif<>('c'), new Unif<>('d'))),
                    new Node<>(new Node<>(new Unif<>('e'), new Unif<>('f')),
                            new Node<>(new Unif<>('g'), new Unif<>('h'))));
            int vector3Size = 8;
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < vector3Size; i++) {
                result.append(vector3.get(vector3Size, i));
            }
            check("abcdefgh".equals(result.toString()), "get returns wrong value");
        }

        // build vector4 and get all its elements
        {
            Tree<Character> vector4 = new Node<>(
                    new Node<>(new Unif<>('a'), new Node<>(new Unif<>('b'), new Unif<>('c'))),
                    new Unif<>('d'));
            int vector4Size = 8;
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < vector4Size; i++) {
                result.append(vector4.get(vector4Size, i));
            }
            check("aabcdddd".equals(result.toString()), "get returns wrong value");
        }

        // build vector5 and get all its elements
        {
            Tree<Character> vector5 = new Node<>(new Unif<>('a'),
                    new Node<>(new Unif<>('a'), new Unif<>('b')));
            int vector5Size = 4;
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < vector5Size; i++) {
                result.append(vector5.get(vector5Size, i));
            }
            check("aaab".equals(result.toString()), "get returns wrong value");
        }

        System.out.println("DONE!\n");

        /*************************************************************************/

        // Complete set methods of classes SparseVector and Unif

        System.out.println("testing set method of SparseVector and Unif");

        // build a vector with all elements equal and set one element to a
        // different value
        {
            for (int i = 0; i < VECTOR_SIZE; i++) {
                SparseVector<Character> v = new SparseVector<>(N, INITIAL_ELEM);
                v.set(i, 'b');
                check(VECTOR_SIZE == v.size(), "vector has wrong size");
                for (int j = 0; j < v.size(); j++) {
                    if (j == i) {
                        check('b' == v.get(j), "get returns wrong value");
                    } else {
                        check(INITIAL_ELEM == v.get(j), "get returns wrong value");
                    }
                }
            }
        }

        // build a vector with all elements equal and then set one element to
        // the same value (thus set has no effect)
        {
            for (int i = 0; i < VECTOR_SIZE; i++) {
                SparseVector<Character> v = new SparseVector<>(N, INITIAL_ELEM);
                v.set(i, INITIAL_ELEM);
                check(VECTOR_SIZE == v.size(), "vector has wrong size");
                for (int j = 0; j < v.size(); j++) {
                    check(INITIAL_ELEM == v.get(j), "get returns wrong value");
                }
            }
        }

        System.out.println("DONE!\n");

        /*************************************************************************/

        // Complete simplify method of class Node

        System.out.println("testing simplify method of Node");

        // Simplify Two Unif Vectors With Same Values
        {
            Node<Character> n = new Node<>(new Unif<>(INITIAL_ELEM), new Unif<>(INITIAL_ELEM));
            Tree<Character> t = n.simplify();
            check(t instanceof Unif<?>, "Tree should be an instance of Unif");
            check(INITIAL_ELEM == t.get(1, 0), "");
        }

        // Simplify Two Unif Vectors With DifferentValues
        {
            Node<Character> n = new Node<>(new Unif<>(INITIAL_ELEM),
                    new Unif<>((char) (INITIAL_ELEM + 1)));
            Tree<Character> t = n.simplify();
            check(t == n, "Tree cannot be simplified, should remain unaltered");
        }

        // Simplify Two Vectors With Different Values
        {
            Node<Character> un = new Node<>(new Unif<>(INITIAL_ELEM), new Node<>(null, null));
            check(un == un.simplify(), "Tree cannot be simplified, should remain unaltered");

            Node<Character> nu = new Node<>(new Node<>(null, null), new Unif<>(INITIAL_ELEM));
            check(nu == nu.simplify(), "Tree cannot be simplified, should remain unaltered");

            Node<Character> nn = new Node<>(new Node<>(null, null), new Node<>(null, null));
            check(nn == nn.simplify(), "Tree cannot be simplified, should remain unaltered");
        }

        System.out.println("DONE!\n");

        /*************************************************************************/

        // Complete set method of class Node

        System.out.println("testing set method of Node");

        // Set First Element Of Vector2
        {
            Tree<Character> vector2 = new Node<>(new Unif<>('a'), new Unif<>('b'));
            int vector2Size = 8;
            vector2.set(vector2Size, 0, 'c');
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < vector2Size; i++) {
                result.append(vector2.get(vector2Size, i));
            }
            check("caaabbbb".equals(result.toString()), "get returns wrong value");
        }

        // Set Third Element Of Vector2
        {
            Tree<Character> vector2 = new Node<>(new Unif<>('a'), new Unif<>('b'));
            int vector2Size = 8;
            vector2.set(vector2Size, 3, 'c');
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < vector2Size; i++) {
                result.append(vector2.get(vector2Size, i));
            }
            check("aaacbbbb".equals(result.toString()), "get returns wrong value");
        }

        // Set Fourth Element Of Vector2
        {
            Tree<Character> vector2 = new Node<>(new Unif<>('a'), new Unif<>('b'));
            int vector2Size = 8;
            vector2.set(vector2Size, 4, 'c');
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < vector2Size; i++) {
                result.append(vector2.get(vector2Size, i));
            }
            check("aaaacbbb".equals(result.toString()), "get returns wrong value");
        }

        // Set Last Element Of Vector2
        {
            Tree<Character> vector2 = new Node<>(new Unif<>('a'), new Unif<>('b'));
            int vector2Size = 8;
            vector2.set(vector2Size, vector2Size - 1, 'c');
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < vector2Size; i++) {
                result.append(vector2.get(vector2Size, i));
            }
            check("aaaabbbc".equals(result.toString()), "get returns wrong value");
        }

        // Set All Elements Of Vector3
        {
            int vector3Size = 8;
            String elements = "abcdefgh";
            for (int i = 0; i < vector3Size; i++) {
                Tree<Character> vector3 = new Node<>(
                        new Node<>(new Node<>(new Unif<>('a'), new Unif<>('b')),
                                new Node<>(new Unif<>('c'), new Unif<>('d'))),
                        new Node<>(new Node<>(new Unif<>('e'), new Unif<>('f')),
                                new Node<>(new Unif<>('g'), new Unif<>('h'))));
                vector3.set(vector3Size, i, '_');
                StringBuilder result = new StringBuilder();
                for (int j = 0; j < vector3Size; j++) {
                    result.append(vector3.get(vector3Size, j));
                }
                String expected = elements.replace(elements.charAt(i), '_');
                check(expected.equals(result.toString()), "get returns wrong value");
            }
        }

        // Set Last Element Of Vector5
        {
            Tree<Character> vector5 = new Node<>(new Unif<>('a'),
                    new Node<>(new Unif<>('a'), new Unif<>('b')));
            int vector5Size = 4;
            vector5.set(vector5Size, vector5Size - 1, 'a');
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < vector5Size; i++) {
                result.append(vector5.get(vector5Size, i));
            }
            check("aaaa".equals(result.toString()), "get returns wrong value");
        }

        System.out.println("DONE!\n");

        /*************************************************************************/

        // Complete SparseVector iterator

        System.out.println("testing SparseVector iterator");

        // Iterate Over Singleton Returns Only Element
        {
            SparseVector<Character> singleton = new SparseVector<>(0, INITIAL_ELEM);
            Iterator<Character> iter = singleton.iterator();
            check(INITIAL_ELEM == iter.next(), "iterator returns wrong value");
            check(!iter.hasNext(), "iterator should be exhausted");
        }

        // Iterate Over Vector1 Returns Same Element
        {
            Iterator<Character> iter = vector1.iterator();
            for (int i = 0; i < VECTOR_SIZE; i++) {
                check(INITIAL_ELEM == iter.next(), "iterator returns wrong value");
            }
            check(!iter.hasNext(), "iterator should be exhausted");
        }

        // Iterate Over Vector Returns Its Contents()
        {
            String message = "abbcccddddefgggh";
            SparseVector<Character> v = new SparseVector<>(4, INITIAL_ELEM);
            for (int i = 0; i < message.length(); i++) {
                v.set(i, message.charAt(i));
            }
            check(message.length() == v.size(), "vector has wrong size");
            Iterator<Character> iter = v.iterator();
            for (int i = 0; i < v.size(); i++) {
                check(message.charAt(i) == iter.next(), "iterator returns wrong value");
            }
            check(!iter.hasNext(), "iterator should be exhausted");
        }

        System.out.println("DONE!\n");
    }

    /**
     * Checks whether {@code condition} is {@code true} or {@false}. If it is
     * {@code true}, does nothing; otherwise throws an exception with the
     * appropriate error {@code message}}.
     *
     * @param condition
     *            an assertion expected to be true
     * @param message
     *            error message
     */
    public static void check(boolean condition, String message) {
        if (!condition) {
            throw new RuntimeException(message);
        }
    }

}
