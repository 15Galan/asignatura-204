import dataStructures.stack.ArrayStack;
import dataStructures.stack.Stack;
import java.util.Iterator;
import java.util.NoSuchElementException;


public class ReverseIterator<T> implements Iterator<T> {

	// ..
	
	/** 
	 * Crea un ReverseIterator a partir de un iterable
	 * Esta clase implementa la interfaz Iterator<T>
	 * @param it  El iterable que se quiere invertir
	 */
	public ReverseIterator(Iterable<T> it) {
		// ...
	}
	
	/**
	 * Hay más elementos en el iterador?
	 */
	public boolean hasNext() {
		return null;
	}
	
	/**
	 * Siguiente elemento del iterador.
	 * Avanza el iterador
	 * @ return El siguiente elemento
	 */
	public T next() {
		// ...
		return null;
	}
	
	/**
	 * Elimina el elemento actual de la estructura sobre la que se itera
	 *
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * Crea y devuelve un iterable que utiliza un reverse iterator 
	 * @param iterable   El iterador a invertir
	 * @return  Un iterable invertido del que se pasa como parámetro
	 */
	public static <T> Iterable<T> elements(final Iterable<T> iterable) {
		// ..
		return null;
	}
}
