/**
 * Student's name:
 *
 * Student's group:
 */

package dataStructures.graph;

import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.set.HashSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

public class FordFulkerson<V> {
    private WeightedDiGraph<V,Integer> g; // Initial graph 
    private List<WDiEdge<V,Integer>> sol; // List of edges representing maximal flow graph
    private V src; 			  // Source
    private V dst; 		  	  // Sink
	
    /**
     * Constructors and methods
     */

    public static <V> int maxFlowPath(List<WDiEdge<V,Integer>> path) {
        int max = 0;

        for (WDiEdge<V,Integer> arista : path) {
            if (arista.getWeight() > max) {
                max = arista.getWeight();
            }
        }

        return max;
    }

    public static <V> List<WDiEdge<V,Integer>> updateEdge(V x, V y, Integer p, List<WDiEdge<V,Integer>> edges) {
        List<WDiEdge<V,Integer>> res = new LinkedList<>();

        for (WDiEdge<V,Integer> arista : edges) {
            if (arista.getSrc() == x && arista.getDst() == y) {
                int peso = arista.getWeight();

                if (arista.getWeight() + p != 0) {
                    res.append(new WDiEdge<>(arista.getSrc(), peso + p, arista.getDst()));
                }

            }
        }

        return res;
    }

    public static <V> List<WDiEdge<V,Integer>> updateEdges(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> edges) {
        List<WDiEdge<V,Integer>> res;
        WDiEdge<V,Integer> edge = edges.get(0);

        res = updateEdge(edge.getSrc(), edge.getDst(), p, path);

        int i = 1;

        while(i < edges.size()){
            res = updateEdge(edges.get(i).getSrc(), edges.get(i).getDst(), p, res);
        }

        return res;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlow(V x, V y, Integer p, List<WDiEdge<V,Integer>> sol) {
        // TO DO
        List<WDiEdge<V,Integer>> res = new LinkedList<>();
        WDiEdge<V,Integer> arco = null;

        for (WDiEdge<V,Integer> arista : sol) {
            if (arista.getSrc() == x && arista.getDst() == y) {
                res = updateEdge(arista.getSrc(), arista.getDst(), p, sol);

            } else if (arista.getSrc() == y && arista.getDst() == x) {

            }
        }


        return null;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlows(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> sol) {
        // TO DO
        return null;
    }

    public FordFulkerson(WeightedDiGraph<V,Integer> g, V src, V dst) {
        // TO DO
    }

    public int maxFlow() {
        // TO DO
        return 0;
    }

    public int maxFlowMinCut(Set<V> set) {
        // TO DO
        return 0;
    }

    /**
     * Provided auxiliary methods
     */
    public List<WDiEdge<V, Integer>> getSolution() {
        return sol;
    }
	
    /**********************************************************************************
     * A partir de aquí SOLO para estudiantes a tiempo parcial sin evaluación continua.
     * ONLY for part time students.
     * ********************************************************************************/

    public static <V> boolean localEquilibrium(WeightedDiGraph<V,Integer> g, V src, V dst) {
        // TO DO
        return false;
    }
    public static <V,W> Tuple2<List<V>,List<V>> sourcesAndSinks(WeightedDiGraph<V,W> g) {
        // TO DO
        return null;
    }

    public static <V> void unifySourceAndSink(WeightedDiGraph<V,Integer> g, V newSrc, V newDst) {
        // TO DO
    }
}
