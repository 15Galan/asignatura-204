import dataStructures.graph.DictionaryGraph;
import dataStructures.graph.Graph;

public class MainUtilGraph {
	public static void main(String[] args) {

		Graph<String> g = new DictionaryGraph<String>();

		g.addVertex("A");
		g.addVertex("B");
		g.addVertex("C");
		g.addVertex("D");
		g.addVertex("E");
		g.addVertex("F");
		g.addVertex("G");
	
		g.addEdge("A", "B");
 		g.addEdge("A", "D");
 		g.addEdge("B", "C");
 		g.addEdge("B", "D");
 		g.addEdge("B", "E");
 		g.addEdge("C", "E");
 		g.addEdge("E", "F");
 		g.addEdge("F", "G");

		for (String v : g.vertices()) {
			System.out.println(v + " " + GraphUtil.eccentricity(g, v));
		}
		System.out.println(GraphUtil.diameter(g));
		
		// Prueba de Reverse Iterator
		Iterator<String> it = new ReverseIterator<String>(g.vertices());
		while (it.hasNext()) {
			System.out.print(it.next());
		}
	}
}
