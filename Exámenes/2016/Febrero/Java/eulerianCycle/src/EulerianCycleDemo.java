import dataStructures.graph.DictionaryGraph;
import dataStructures.graph.EulerianCycle;
import dataStructures.graph.Graph;
import dataStructures.list.List;

public class EulerianCycleDemo {

    public static void main(String[] args) throws InterruptedException {

        Graph<Character> g2n = new DictionaryGraph<>(); // not eulerian

        g2n.addVertex('A');
        g2n.addVertex('B');
        g2n.addEdge('A', 'B');

        Graph<Character> g3 = new DictionaryGraph<>(); // eulerian

        g3.addVertex('A');
        g3.addVertex('B');
        g3.addVertex('C');

        g3.addEdge('A', 'B');
        g3.addEdge('B', 'C');
        g3.addEdge('C', 'A');

        Graph<Character> g5 = new DictionaryGraph<>(); // eulerian

        for (Character c = 'A'; c <= 'E'; c++) {
            g5.addVertex(c);
        }

        g5.addEdge('A', 'C');
        g5.addEdge('A', 'D');
        g5.addEdge('B', 'C');
        g5.addEdge('B', 'E');
        g5.addEdge('C', 'D');
        g5.addEdge('C', 'E');

        Graph<Character> g5n = new DictionaryGraph<>(); // eulerian

        for (Character c = 'A'; c <= 'E'; c++) {
            g5n.addVertex(c);
        }

        g5n.addEdge('A', 'C');
        g5n.addEdge('A', 'D');
        g5n.addEdge('B', 'C');
        g5n.addEdge('B', 'E');
        g5n.addEdge('C', 'D');
        g5n.addEdge('C', 'E');
        g5n.addEdge('D', 'E');

        Graph<Character> g6 = new DictionaryGraph<>(); // eulerian

        for (char c = 'A'; c <= 'F'; c++) {
            g6.addVertex(c);
        }

        g6.addEdge('A', 'B');
        g6.addEdge('A', 'E');
        g6.addEdge('B', 'C');
        g6.addEdge('B', 'D');
        g6.addEdge('B', 'E');
        g6.addEdge('C', 'D');
        g6.addEdge('C', 'E');
        g6.addEdge('C', 'F');
        g6.addEdge('D', 'E');
        g6.addEdge('D', 'F');

        Graph<Character> g10 = new DictionaryGraph<>(); // eulerian

        for (char c = 'A'; c <= 'J'; c++) {
            g10.addVertex(c);
        }

        g10.addEdge('A', 'E');
        g10.addEdge('A', 'J');
        g10.addEdge('A', 'I');
        g10.addEdge('A', 'B');
        g10.addEdge('B', 'I');
        g10.addEdge('B', 'H');
        g10.addEdge('B', 'C');
        g10.addEdge('C', 'D');
        g10.addEdge('C', 'G');
        g10.addEdge('C', 'H');
        g10.addEdge('C', 'B');
        g10.addEdge('D', 'E');
        g10.addEdge('D', 'F');
        g10.addEdge('D', 'G');
        g10.addEdge('E', 'F');
        g10.addEdge('E', 'J');
        g10.addEdge('F', 'G');
        g10.addEdge('F', 'J');
        g10.addEdge('G', 'H');
        g10.addEdge('H', 'I');
        g10.addEdge('I', 'J');

        Graph<Character> g10n = new DictionaryGraph<>(); // not eulerian

        for (char c = 'A'; c <= 'J'; c++) {
            g10n.addVertex(c);
        }

        g10n.addEdge('A', 'E');
        g10n.addEdge('A', 'J');
        g10n.addEdge('A', 'I');
        g10n.addEdge('A', 'B');
        g10n.addEdge('B', 'I');
        g10n.addEdge('B', 'H');
        g10n.addEdge('B', 'C');
        g10n.addEdge('C', 'D');
        g10n.addEdge('C', 'G');
        g10n.addEdge('C', 'H');
        g10n.addEdge('C', 'B');
        // g11.addEdge('D', 'E');
        g10n.addEdge('D', 'F');
        g10n.addEdge('D', 'G');
        g10n.addEdge('E', 'F');
        g10n.addEdge('E', 'J');
        g10n.addEdge('F', 'G');
        g10n.addEdge('F', 'J');
        g10n.addEdge('G', 'H');
        g10n.addEdge('H', 'I');
        g10n.addEdge('I', 'J');

        Object[] graphs = new Object[] { g2n, g3, g5, g5n, g6, g10, g10n };
        String[] names = new String[] { "g2n", "g3", "g5", "g5n", "g6", "g10", "g10n" };

        for (int i = 0; i < graphs.length; i++) {
            @SuppressWarnings("unchecked")
            Graph<Character> g = (Graph<Character>) graphs[i];
            System.out.println("Test for graph " + names[i]);

            EulerianCycle<Character> ec = new EulerianCycle<>(g);
            if (ec.isEulerian()) {
                System.out.println("length of cycle: " + ec.eulerianCycle().size());
                System.out.println("cycle: " + ec.eulerianCycle());
                checkEulerianCycle(names[i], g, ec.eulerianCycle());

            } else {
                System.out.println("Graph is not Eulerian");
            }

            System.out.println("-----------------------");
        }

        System.out.println("DONE!");
    }

    private static <V> void checkEulerianCycle(String nombre, Graph<V> g, List<V> cycle) {
        if (g.numEdges() != cycle.size() - 1) {
            System.err.println(nombre + "\twrong cycle length");
        }
        if (!(cycle.get(0)).equals(cycle.get(cycle.size() - 1))) {
            System.err.println(nombre + "\tpath is not closed");
        }
        for (int i = 0; i < cycle.size() - 2; i++) {
            V from = cycle.get(i);
            V to = cycle.get(i + 1);
            if (!g.successors(from).isElem(to)) {
                System.err.println(nombre + "\twrong edge: " + from + " -> " + to);
            }
        }

        V origin = cycle.get(0);
        if (g.degree(origin) != 2 * (occurrences(origin, cycle) - 1)) {
            System.err.println(nombre + "\twrong number of occurrences for origin vertex: " + origin);
        }
        for (V v : g.vertices()) {
            if (!v.equals(origin) && g.degree(v) != 2 * occurrences(v, cycle)) {
                System.err.println(nombre + "\twrong number of occurrences for vertex: " + v);
            }
        }
    }

    private static <V> int occurrences(V elem, List<V> list) {
        int count = 0;
        for (V e : list) {
            if (elem.equals(e)) {
                count++;
            }
        }
        return count;
    }
}
