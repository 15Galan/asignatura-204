/**
 * Student's name:
 *
 * Student's group:
 */

import dataStructures.list.ArrayList;
import dataStructures.list.List;
import dataStructures.list.LinkedList;
import dataStructures.list.ListException;

import java.util.Iterator;


class Bin {
    private int remainingCapacity; // capacity left for this bin
    private List<Integer> weights; // weights of objects included in this bin

    public Bin(int initialCapacity) {
        remainingCapacity = initialCapacity;
        weights = new LinkedList<>();
    }

    // returns capacity left for this bin
    public int remainingCapacity() {

        return remainingCapacity;
    }

    // adds a new object to this bin
    public void addObject(int weight) {
        if (remainingCapacity < weight) {
            throw new ListException("Error con la capadidad");

        } else {
            remainingCapacity -= weight;
            weights.append(weight);
        }
    }

    // returns an iterable through weights of objects included in this bin
    public Iterable<Integer> objects() {
        // todo
        //  SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
        //  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
        return null;
    }

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        sb.append(remainingCapacity);
        sb.append(", ");
        sb.append(weights.toString());
        sb.append(")");
        return sb.toString();
    }
}

// Class for representing an AVL tree of bins
public class AVL {
    static private class Node {
        Bin bin; // Bin stored in this node
        int height; // height of this node in AVL tree
        int maxRemainingCapacity; // max capacity left among all bins in tree rooted at this node
        Node left, right; // left and right children of this node in AVL tree

        // recomputes height of this node
        void setHeight() {

            if (left == null && right == null) {
                height = 1;

            } else if (left == null) {
                height = 1 + right.height;

            } else if (right == null) {
                height = 1 + left.height;

            } else {
                height = 1 + Math.max(left.height, right.height);
            }
        }

        // recomputes max capacity among bins in tree rooted at this node
        void setMaxRemainingCapacity() {

            if (left == null && right == null) {
                maxRemainingCapacity = bin.remainingCapacity();

            } else if (left == null) {
                maxRemainingCapacity = right.maxRemainingCapacity;

            } else if (right == null) {
                maxRemainingCapacity = left.maxRemainingCapacity;

            } else {
                maxRemainingCapacity = Math.max(left.maxRemainingCapacity, right.maxRemainingCapacity);
            }
        }

        // left-rotates this node. Returns root of resulting rotated tree
        Node rotateLeft() {
            Node res = this;

            if (left == null) {
                res.left = this;
                res.right = null;

            } else {
                res.right = right.right;
                res.left.right = right.left;
                res.left.left = left;
            }

            res.setHeight();
            res.setMaxRemainingCapacity();

            return res;
        }
    }

    private static int height(Node node) {
        return node.height;
    }

    private static int maxRemainingCapacity(Node node) {

        return node.maxRemainingCapacity;
    }

    private Node root; // root of AVL tree

    public AVL() {
        this.root = null;
    }

    // adds a new bin at the end of right spine.
    private void addNewBin(Bin bin) {
        // todo
        Node nodo = new Node();

        if (this.root.right == null) {
            nodo.left = null;
            nodo.right = null;

            nodo.bin = bin;

            nodo.setHeight();
            nodo.setMaxRemainingCapacity();

//            if (nodo.right.height > (nodo.right.height +1)) {
//                nodo.rotateLeft();
//            }

        } else {
            this.root = this.root.right;
            addNewBin(bin);

//            if (this.root.right.height > (this.root.right.height +1)) {
//                this.root.rotateLeft();
//            }
        }
    }

    // adds an object to first suitable bin. Adds
    // a new bin if object cannot be inserted in any existing bin
    public void addFirst(int initialCapacity, int weight) {
        // todo
        Node nodo = new Node();

        Bin bin = new Bin(initialCapacity);
        bin.addObject(weight);

        nodo.bin = bin;

        if (this.root == null) {
            nodo.left = null;
            nodo.right = null;

            nodo.setMaxRemainingCapacity();
            nodo.setHeight();

        } else if (maxRemainingCapacity(root.left) >= maxRemainingCapacity(root.right)) {


        } else if (maxRemainingCapacity(root) >= weight) {


        } else {
            addNewBin(bin);
        }
    }

    public void addAll(int initialCapacity, int[] weights) {

        for (int w : weights) {
            addFirst(initialCapacity, w);
        }
    }

    public List<Bin> toList() {
        // todo
        List<Bin> bins = new ArrayList<>();

        // Izquierda
        if (this.root.left == null) {
            bins.append(this.root.bin);

        } else {
            this.root = this.root.left;
            toList();
        }

        // Derecha
        if (this.root.right == null) {
            bins.append(this.root.bin);

        } else {
            this.root = this.root.right;
            toList();
        }

        return bins;
    }

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        stringBuild(sb, root);
        sb.append(")");
        return sb.toString();
    }

    private static void stringBuild(StringBuilder sb, Node node) {
        if(node==null)
            sb.append("null");
        else {
            sb.append(node.getClass().getSimpleName());
            sb.append("(");
            sb.append(node.bin);
            sb.append(", ");
            sb.append(node.height);
            sb.append(", ");
            sb.append(node.maxRemainingCapacity);
            sb.append(", ");
            stringBuild(sb, node.left);
            sb.append(", ");
            stringBuild(sb, node.right);
            sb.append(")");
        }
    }
}

class LinearBinPacking {
    public static List<Bin> linearBinPacking(int initialCapacity, List<Integer> weights) {
        // todo
        //  SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
        //  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
        return null;
    }
	
	public static Iterable<Integer> allWeights(Iterable<Bin> bins) {
        // todo
        //  SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
        //  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
        return null;		
	}
}