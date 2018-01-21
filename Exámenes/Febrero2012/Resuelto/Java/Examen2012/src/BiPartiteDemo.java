/**
 * @author Blas Ruiz, Data Structures, Grado en Informática. UMA.
 *
 * Control 2. 13 Febrero 2012. Bipartite Demo  
 */

import graph.Graph;
import graph.DictionaryGraph;
import graph.BiPartite;

public class BiPartiteDemo {

	// construcción del grafo bipartito k(n,m)  
	private static Graph<Integer> k(int n, int m){
		Graph<Integer> graph = new DictionaryGraph<Integer>();
        
		for (int r = 1; r<=n; r++){
			for (int a = n+1; a<=n+m; a++){
				// addEddge añade los vértices si fuera necesario
				graph.addEdge(r, a);
			}
		}
		return graph;
	}

	private static <V> void test (Graph<V> g){
		System.out.println(g);

		BiPartite<V> testBiPartite  = new BiPartite<V>(g);
	
 		if(testBiPartite.isBicolored()) {
 			System.out.println("BiColoreado del anterior grafo:");
			System.out.println(testBiPartite.biColored());
 		}
		else
			System.out.println("No es bipartito.");
		System.out.println("------------------");
		}

	public static void main(String[] args) {
		Graph<Integer> g = new DictionaryGraph<Integer>();
		
		g.addEdge(1,2);
		g.addEdge(2,3);
		g.addEdge(3,4);
		g.addEdge(4,5);
		g.addEdge(5,6);
//    	g.addEdge(6,4); // con esta arista ya no es bipartito (tiene un ciclo impar)
		g.addEdge(6,1);

		test(g);
		

		System.out.println("Ahora probamos con el completo k(2,4)");
		Graph<Integer> gbp = k(2,4);
		test(gbp);
	
		System.out.println("Ahora añadimos la arista 4-6");
		Graph<Integer> gbp2 = (Graph<Integer>) gbp.clone();
		gbp2.addEdge(4,6);
		test(gbp2);
		
 		

	}	

}
