/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Right case for union type
 */
 
package tree;

import java.util.NoSuchElementException;

public class Right<A,B> implements Either<A,B> {
	private B right;
	
	public Right(B x) { right = x; }

	public boolean isLeft() { return false; }
	public boolean isRight() { return true; }
	public A left() { throw new NoSuchElementException("left on Right object"); }
	public B right() { return right; }
	
	@Override public String toString() { return "Right("+right+")"; }
}
