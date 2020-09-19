import dataStructures.graph.*;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.set.HashSet;
import dataStructures.set.Set;

public class DemoFordFulkerson {

    public static void main(String[] args) {

        WeightedDiGraph<String, Integer> g = new WeightedDictionaryDiGraph<>(); 

        g.addVertex("S");
        g.addVertex("A");
        g.addVertex("B");
        g.addVertex("C");
        g.addVertex("D");
        g.addVertex("E");
        g.addVertex("F");
        g.addVertex("T");

        g.addDiEdge("S", 4, "A");
        g.addDiEdge("S", 8, "B");
        g.addDiEdge("S", 4, "C");
        g.addDiEdge("A", 3, "D");
        g.addDiEdge("A", 7, "F");
        g.addDiEdge("B", 9, "E");
        g.addDiEdge("C", 5, "E");
        g.addDiEdge("C", 2, "F");
        g.addDiEdge("D", 6, "T");
        g.addDiEdge("E", 9, "T");
        g.addDiEdge("F", 5, "T");

        List<WDiEdge<String, Integer>> path = new LinkedList<>();
        path.append(new WDiEdge<>("A",3,"B"));
        path.append(new WDiEdge<>("B",1,"D"));
        path.append(new WDiEdge<>("D",3,"C"));
        System.out.println("Maximal flow in path is "+FordFulkerson.maxFlowPath(path));

        List<WDiEdge<String, Integer>> edges = new LinkedList<>();
        edges.append(new WDiEdge<>("A",2,"B"));
        edges.append(new WDiEdge<>("A",5,"C"));
        edges.append(new WDiEdge<>("B",6,"D"));
        edges.append(new WDiEdge<>("D",1,"E"));
        edges.append(new WDiEdge<>("B",4,"A"));
        System.out.println(FordFulkerson.updateEdge("A","C", 3,edges));
        System.out.println(FordFulkerson.updateEdge("D","B", 2,edges));

        System.out.println(g);

        List<WDiEdge<String, Integer>> wedges = g.wDiEdges();

        FordFulkerson<String> ff = new FordFulkerson<>(g, "S", "T");
        System.out.println("Edges in maximal flow graph: " + ff.getSolution());
        System.out.println("Maximal flow rate = " + ff.maxFlow());
		
        Set<String> set = new HashSet<>();
        set.insert("S");
        set.insert("A");
        set.insert("F");
        System.out.println("Maximal flow rate using set " + set + " is " + ff.maxFlowMinCut(set));
    }
}

/* Expected output:

Maximal flow in path is 1
LinkedList(A-2->B,A-8->C,B-6->D,D-1->E,B-4->A)
LinkedList(A-2->B,A-5->C,B-6->D,D-1->E,B-4->A,D-2->B)
WeightedDictionaryDiGraph(vertices=(A,B,C,D,E,F,S,T), edges=(A-3->D,A-7->F,B-9->E,C-2->F,C-5->E,D-6->T,E-9->T,F-5->T,S-4->A,S-4->C,S-8->B))
Edges in maximal flow graph: LinkedList(S-4->A,A-3->D,D-3->T,A-1->F,F-3->T,S-4->C,C-2->F,C-2->E,E-9->T,S-7->B,B-7->E)
Maximal flow rate = 15
Maximal flow rate using set HashSet(A,F,S) is 15
*/
