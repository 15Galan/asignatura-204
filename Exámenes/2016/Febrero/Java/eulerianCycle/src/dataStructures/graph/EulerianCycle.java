/**
 * Student's name:
 * Student's group:
 *
 * Data Structures. Grado en Informática. UMA.
 */

package dataStructures.graph;

import dataStructures.list.*;

public class EulerianCycle<V> {
    private List<V> eCycle;

    @SuppressWarnings("unchecked")
    public EulerianCycle(Graph<V> g) {
        Graph<V> graph = (Graph<V>) g.clone();
        eCycle = eulerianCycle(graph);
    }

    public boolean isEulerian() {
        return eCycle != null;
    }

    public List<V> eulerianCycle() {
        return eCycle;
    }

    // J.1
    private static <V> boolean isEulerian(Graph<V> g) {
        boolean res = true;

        for (V v : g.vertices()) {
            res = res && (g.degree(v) % 2 == 0);
        }

        return res;
    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u) {
        g.deleteEdge(v, u);

        for (V ver : g.vertices()) {
            if (g.degree(ver) == 0) {
                g.deleteVertex(ver);
            }
        }
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {
        List<V> ciclo = new LinkedList<>();
        V v = v0, u = g.successors(v).iterator().next();

        ciclo.append(v);

        while (v0 != u) {
            remove(g, v, u);
            ciclo.append(u);

            v = u;
            u = g.successors(v).iterator().hasNext() ? g.successors(v).iterator().next() : v0;
        }

        ciclo.append(v0);

        return ciclo;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys) {
        if (xs.isEmpty()) {
            for (V v : ys) {
                xs.append(v);
            }

        } else {
            int i = 0;

            while (xs.get(i) != ys.get(0)) {
                i++;
            }

            for (V v : ys) {
                i++;
                xs.insert(i, v);

            }
        }
    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
        int i = 0;
        V res = null;

        if (cycle.isEmpty()) {
            res = g.vertices().iterator().next();

        } else{
            while (res == null) {    // Asumiendo que siempre hay al menos uno
                V actual = cycle.get(i);
                g.vertices().isElem(actual);

                res = actual;
            }
        }

        return res;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
        List<V> ciclo;

        if (!isEulerian(g)) {
            ciclo = null;       // El test interpreta un ciclo nulo como muetra de no-euleriano

        } else {
            ciclo = new LinkedList<>();

            while (!g.isEmpty()) {
                connectCycles(ciclo, extractCycle(g,vertexInCommon(g, ciclo)));
            }
        }

        return ciclo;
    }
}
