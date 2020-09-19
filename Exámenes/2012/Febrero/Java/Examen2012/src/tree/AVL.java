/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * AVL trees implementation
 */
 
package tree;

import java.util.Iterator;
import java.util.NoSuchElementException;

import tree.Either;
import tree.Left;
import tree.Right;
import stack.StackLink;
import stack.Stack;

public class AVL<K extends Comparable<? super K>, V> implements SearchTree<K, V> {
	private static class Tree<K,V> {
		K key;
		V value;
		int height;
		Tree<K,V> left;
		Tree<K,V> right;

		public Tree(K k, V v) {
			key = k;
			value = v;
			height = 1;
			left = null;
			right = null;
		}
		
		public static int height(Tree<?,?> tree) {
			return tree==null ? 0 : tree.height;
		}
		
		public boolean rightLeaning() {
			return height(right) > height(left);
		}

		public boolean leftLeaning() {
			return height(right) < height(left);
		}

		void setHeight() {
			height = 1 + Math.max(height(left), height(right));
		}
		
		public Tree<K,V> rotR() {
			Tree<K,V> lt = this.left;
			Tree<K,V> lrt = lt.right;
			
			this.left = lrt;
			this.setHeight();
			
			lt.right = this;
			lt.setHeight();
			
			return lt;
		}
		
		public Tree<K,V> rotL() {
			Tree<K,V> rt = this.right;
			Tree<K,V> rlt = rt.left;
			
			this.right = rlt;
			this.setHeight();
			
			rt.left = this;
			rt.setHeight();
			
			return rt;
		}
		
		// balance receiving node. Returns node already balanced
		public Tree<K,V> balance() {
			int lh = height(left);
			int rh = height(right);
			
			Tree<K,V> balanced;
			
			if(lh - rh > 1  && left.rightLeaning()) {
				left = left.rotL();
				balanced = this.rotR();
			} else if (lh - rh > 1) { 
				balanced = this.rotR();
			} else if(rh - lh > 1  && right.leftLeaning()) {
				right = right.rotR();
				balanced = this.rotL();
			} else if (rh - lh > 1) {
				balanced = this.rotL();
			} else {
				balanced = this; //no rotation needed
				balanced.setHeight(); 
			}	
			return balanced;
				
		}

		interface Predicate<T> {
			boolean apply(T x);
		}
		
		public static <K> boolean all(Predicate<K> p, Tree<K,?> tree) {
			if(tree==null)
				return true;
			else
				return (p.apply(tree.key) && all(p,tree.left) && all(p,tree.right));
		}	
	
		public static <K extends Comparable<? super K>> boolean isAVL(final Tree<K,?> tree) {
			if(tree==null)
				return true;
			else {
				Predicate<K> lesser = new Predicate<K>() { 
    	   			public boolean apply(K k){return k.compareTo(tree.key) < 0;}
    	   		};
    	   		
				Predicate<K> greater = new Predicate<K>() { 
    	   			public boolean apply(K k){return k.compareTo(tree.key) > 0;}
    	   		};
    	   		
				return (Math.abs(height(tree.left)-height(tree.right)) < 2)
				        && all(lesser,tree.left) //less(tree.key,tree.left)
				        && all(greater,tree.right) //greater(tree.key,tree.right)
				        && isAVL(tree.left)
				        && isAVL(tree.right);
			}
		}
	}

	private Tree<K,V> root;
	private int size;

	public AVL() {
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
		return Tree.height(root);
	}
	
	public V search(K key) {
		return searchRec(root, key);
	}

	private static <K extends Comparable<? super K>,V>
	        V searchRec(Tree<K,V> tree, K key) {
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
		
	public void insert(K k, V v) {
		root = insertRec(root, k, v);
	}

	// returns modified tree
	private Tree<K,V> insertRec(Tree<K,V> node, K key, V value) {
		if (node == null) {
			node = new Tree<K,V>(key, value);
			size++;
		} else if (key.compareTo(node.key) == 0)
			node.value = value;
		else if (key.compareTo(node.key) < 0) {
			node.left = insertRec(node.left, key, value);
			node = node.balance();
		} else {
			node.right = insertRec(node.right, key, value);
			node = node.balance();
		}	
		return node;
	}
	
	
	
	
	// pre: node is a non-empty tree
	// Removes minimum key (and value) from tree rooted at node. Before
	// deletion, key and value are saved into temp node.
	// returns modified tree (without min key and value)
	private static  <K extends Comparable<? super K>,V>
	        Tree<K,V> split(Tree<K,V> node, Tree<K,V> temp) {
		if (node.left == null) {
			// min node found, so copy min key and value
			temp.key = node.key;
			temp.value = node.value;
			return node.right; // remove node
		} else {
			// remove min from left subtree
			node.left = split(node.left, temp);
			node = node.balance();
			return node;
		}
	}

	public void delete(K key) {
		root = deleteRec(root, key);
		// assert Tree.isAVL(root) : "AVL.delete: !isAVL";
	}

	// returns modified tree
	private Tree<K,V> deleteRec(Tree<K,V> node, K key) {
		if (node == null)
			; // key not found; do nothing
		else if (key.compareTo(node.key) == 0) {
			if (node.left == null)
				node = node.right;
			else if (node.right == null)
				node = node.left;
			else {
				node.right = split(node.right, node);
			  node = node.balance();
			}
			size--;
		} else if (key.compareTo(node.key) < 0) {
			node.left = deleteRec(node.left, key);
			node = node.balance();
		} else {
			node.right = deleteRec(node.right, key);
			node = node.balance();
		}	
		return node;
	}	
	
	
	
	// iterators

	private abstract class Traversal implements Iterator<K> {
		Stack<Either<K, Tree<K,V>>> stack = new StackLink<Either<K, Tree<K,V>>>();

		abstract void save(Tree<K,V> node);

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

			Either<K, Tree<K,V>> either = stack.top();
			stack.pop();

			while (either.isRight()) {
				Tree<K,V> node = either.right();
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
			void save(Tree<K,V> node) {
				// in reverse order, cause stack is LIFO
				if (node.right != null)
					stack.push(new Right<K, Tree<K,V>>(node.right));
				stack.push(new Left<K, Tree<K,V>>(node.key));
				if (node.left != null)
					stack.push(new Right<K, Tree<K,V>>(node.left));
			}
		};
	}

	public Iterator<K> postOrder() {
		return new Traversal() {
			void save(Tree<K,V> node) {
				// in reverse order, cause stack is LIFO
				stack.push(new Left<K, Tree<K,V>>(node.key));
				if (node.right != null)
					stack.push(new Right<K, Tree<K,V>>(node.right));
				if (node.left != null)
					stack.push(new Right<K, Tree<K,V>>(node.left));
			}
		};
	}

	public Iterator<K> preOrder() {
		return new Traversal() {
			void save(Tree<K,V> node) {
				// in reverse order, cause stack is LIFO
				if (node.right != null)
					stack.push(new Right<K, Tree<K,V>>(node.right));
				if (node.left != null)
					stack.push(new Right<K, Tree<K,V>>(node.left));
				stack.push(new Left<K, Tree<K,V>>(node.key));
			}
		};
	}

	public Iterator<K> iterator() {
		return inOrder();
	}
}