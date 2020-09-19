/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Left case for union type
 */

package tree;

import java.util.NoSuchElementException;

public class Left<A,B> implements Either<A,B> {
	private A left;
	
	public Left(A x) { left = x; }
	
	public boolean isLeft() { return true; }
	public boolean isRight() { return false; }
	public A left() { return left; }
	public B right() { throw new NoSuchElementException("right on Left object"); }
	
	@Override public String toString() { return "Left("+left+")"; }
}
