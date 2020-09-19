/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Left case for union type
 */

package graph;

public class Pair<A,B> {
	private A a;
	private B b;
	
	public Pair(A x,B y) {a = x; b = y;}
	
	public A first() { return a; }
	public B second() { return b;}
	public A fst() { return a; } // las de haskell
	public B snd() { return b; }

	@Override public String toString() { return "Pair(" + a + "," + b + ")";}
}
