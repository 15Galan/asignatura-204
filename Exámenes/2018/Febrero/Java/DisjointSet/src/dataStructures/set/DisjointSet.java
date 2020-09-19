package dataStructures.set;

import dataStructures.list.List;

public interface DisjointSet<T extends Comparable<? super T>> {

    /**
     * Devuelve {@code true} si el conjunto no contiene elementos.
     */
    public boolean isEmpty();

    /**
     * Devuelve {@code true} si {@code elem} es un elemento del conjunto.
     */
    boolean isElem(T elem);

    /**
     * Devuelve el numero total de elementos del conjunto.
     */
    int numElements();

    /**
     * Agrega {@code elem} al conjunto. Si {@code elem} no pertenece al
     * conjunto, crea una nueva clase de equivalencia con {@code elem}. Si
     * {@code elem} pertencece al conjunto no hace nada.
     */
    void add(T elem);

    /**
     * Devuelve {@code true} si {@code elem1} y {@code elem2} estan en la misma
     * clase de equivalencia.
     */
    boolean areConnected(T elem1, T elem2);

    /**
     * Devuelve una lista con los elementos pertenecientes a la clase de
     * equivalencia en la que esta {@code elem}. Si {@code elem} no pertenece al
     * conjunto devuelve la lista vacia.
     */
    List<T> kind(T elem);

    /**
     * Une las clases de equivalencias de {@code elem1} y {@code elem2}. Si
     * alguno de los dos argumentos no esta en el conjunto lanzara una excepcion
     * {@code IllegalArgumenException}.
     */
    void union(T elem1, T elem2);

    /**
     * Aplana la estructura de manera que todos los elementos se asocien
     * directamente con su representante canonico.
     */
    void flatten();

    /**
     * Devuelve una lista que contiene las clases de equivalencia del conjunto
     * como listas.
     */
    List<List<T>> kinds();
}
