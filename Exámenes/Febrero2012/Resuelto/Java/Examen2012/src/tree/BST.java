/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * Binary Search trees implementation
 */
 
 package tree;

import java.util.Iterator;
import java.util.NoSuchElementException;


import stack.StackLink;
import stack.Stack;

public class BST<K extends Comparable<? super K>, V> implements SearchTree<K, V> {
	private static class Tree<K, V> {
		K key;
		V value;
		Tree<K, V> left;
		Tree<K, V> right;

		public Tree(K k, V v) {
			key = k;
			value = v;
			left = null;
			right = null;
		}
	}

	private Tree<K, V> root;
	private int size;

	public BST() {
		root = null;
		size = 0;
	}

	public boolean isEmpty() {
		return root == null;
	}

	public int size() {
		return size;
	}	
	
	
	public int height() {
		return heightRec(root);
	}	
	
	private static int heightRec(Tree<?,?> tree) {
		return tree==null ? 0 : 1 + Math.max(heightRec(tree.left), heightRec(tree.right));
	}	
	
	public void insert(K k, V v) {
		root = insertRec(root, k, v);
	}

	// returns modified tree
	private Tree<K, V> insertRec(Tree<K, V> node, K key, V value) {
		if (node == null) {
			node = new Tree<K, V>(key, value);
			size++;
		} else if (key.compareTo(node.key) == 0)
			node.value = value;
		else if (key.compareTo(node.key) < 0)
			node.left = insertRec(node.left, key, value);
		else
			node.right = insertRec(node.right, key, value);
		return node;
	}

	public V search(K key) {
		return searchRec(root, key);
	}

	private static <K extends Comparable<? super K>,V> 
	        V searchRec(Tree<K, V> tree, K key) {
		if (tree == null)
			return null; 
		else if (key.compareTo(tree.key) == 0)
			return tree.value;
		else if (key.compareTo(tree.key) < 0)
			return searchRec(tree.left, key);
		else
			return searchRec(tree.right, key);
	}

	public boolean isElem(K key) {
		return search(key) != null;
	}

	// pre: node is a non-empty tree
	// Removes minimum key (and value) from tree rooted at node. Before
	// deletion, key and value are saved into temp node.
	// returns modified tree (without min key and value)
	private static  <K extends Comparable<? super K>,V> 
	        Tree<K, V> split(Tree<K, V> node, Tree<K, V> temp) {
		if (node.left == null) {
			// min node found, so copy min key and value
			temp.key = node.key;
			temp.value = node.value;
			return node.right; // remove node
		} else {
			// remove min from left subtree
			node.left = split(node.left, temp);
			return node;
		}
	}

	public void delete(K key) {
		root = deleteRec(root, key);
	}

	// returns modified tree
	private Tree<K, V> deleteRec(Tree<K, V> node, K key) {
		if (node == null)
			; // key not found; do nothing
		else if (key.compareTo(node.key) == 0) {
			if (node.left == null)
				node = node.right;
			else if (node.right == null)
				node = node.left;
			else
				node.right = split(node.right, node);
			size--;
		} else if (key.compareTo(node.key) < 0)
			node.left = deleteRec(node.left, key);
		else
			node.right = deleteRec(node.right, key);
		return node;
	}

	
	// iterators
	private abstract class Traversal implements Iterator<K> {
		Stack<Either<K, Tree<K, V>>> stack = new StackLink<Either<K, Tree<K, V>>>();

		abstract void save(Tree<K, V> node);

		public Traversal() {
			if (root != null)
				save(root);
		}

		public boolean hasNext() {
			return !stack.isEmpty();
		}

		public K next() {
			if (!hasNext())
				throw new NoSuchElementException();

			Either<K, Tree<K, V>> either = stack.top();
			stack.pop();

			while (either.isRight()) {
				Tree<K, V> node = either.right();
				save(node);
				either = stack.top();
				stack.pop();
			}
			return either.left();
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	public Iterator<K> inOrder() {
		return new Traversal() {
			void save(Tree<K, V> node) {
				// in reverse order, cause stack is LIFO
				if (node.right != null)
					stack.push(new Right<K, Tree<K, V>>(node.right));
				stack.push(new Left<K, Tree<K, V>>(node.key));
				if (node.left != null)
					stack.push(new Right<K, Tree<K, V>>(node.left));
			}
		};
	}

	
	class PreOrder extends Traversal {
		void save(Tree<K, V> node) {
			// in reverse order, cause stack is LIFO
			if (node.right != null)
				stack.push(new Right<K, Tree<K, V>>(node.right));
			stack.push(new Left<K, Tree<K, V>>(node.key));
			if (node.left != null)
				stack.push(new Right<K, Tree<K, V>>(node.left));
		}		
	}

	public Iterator<K> postOrder() {
		return new Traversal() {
			void save(Tree<K, V> node) {
				// in reverse order, cause stack is LIFO
				stack.push(new Left<K, Tree<K, V>>(node.key));
				if (node.right != null)
					stack.push(new Right<K, Tree<K, V>>(node.right));
				if (node.left != null)
					stack.push(new Right<K, Tree<K, V>>(node.left));
			}
		};
	}

	public Iterator<K> preOrder() {
		return new Traversal() {
			void save(Tree<K, V> node) {
				// in reverse order, cause stack is LIFO
				if (node.right != null)
					stack.push(new Right<K, Tree<K, V>>(node.right));
				if (node.left != null)
					stack.push(new Right<K, Tree<K, V>>(node.left));
				stack.push(new Left<K, Tree<K, V>>(node.key));
			}
		};
	}

	public Iterator<K> iterator() {
		return inOrder();
	}
}

