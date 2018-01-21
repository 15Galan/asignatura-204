/**
 * @author Blas Ruiz, Data Structures, Grado en Inform‡tica. UMA.
 *
 * Control 2. 13-Febrero-2012
 * Estudio de grafos bipartitos por coloreado con búsqueda en profundidad
 */

package graph;

import dictionary.Dictionary;
import dictionary.HashDictionary;
import stack.Stack;
import stack.StackList;


public class BiPartite<V> {
	
	public static enum Color {Red, Blue;
	}

	private static Color nextColor(Color c) {
		return (c == Color.Blue) ?Color.Red:Color.Blue; 
	}
	
	private Stack<Pair<V,Color>> stack; // stack with pair of vertex and color
	private Dictionary<V,Color> dict;  // dictionary: Vertices -> Color
	private boolean isBiColored;

	public BiPartite(Graph<V> graph) {
		dict  = new HashDictionary<V, Color>();
		stack = new StackList<Pair<V, Color>>();
		isBiColored = true;

		if (graph.numVertices() == 0)
			return; 

		V src = graph.vertices().iterator().next(); // initial vertex
		
		stack.push(new Pair<V,Color>(src,Color.Red));
		
		while (!stack.isEmpty()) {
			Pair<V,Color> vColor = stack.top();     // (a)
			stack.pop();                           // (a)

            V v     = vColor.fst();
            Color c = vColor.snd();

            if(dict.valueOf(v) == null){    // Vértice NO visitado
                dict.insert(v,c);

                for(V suc : graph.successors(vColor.fst())){    // Insertar vértices sucesores en una pila.
                    if(dict.valueOf(suc) == null){             // Si un sucesor NO fue visitado:

                        Color c2 = nextColor(c);                    // Cambiar color del sucesore.
                        stack.push(new Pair<V,Color>(suc,c2));     // Insertarlo en la pila.
                    }
                }

            }else{
                isBiColored = (dict.valueOf(v) != c);
            }
		} 
	}	
	
	public Dictionary<V,Color> biColored() {
		return dict;
	}
	
	public boolean isBicolored() {
		return isBiColored;
	}
	
}
