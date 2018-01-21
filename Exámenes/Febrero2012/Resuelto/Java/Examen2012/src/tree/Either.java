/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Union type
 */

package tree;

public interface Either<A, B> {
	boolean isLeft();
	boolean isRight();
	
	A left();
	B right();
}

