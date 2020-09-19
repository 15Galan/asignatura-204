import dataStructures.set.DisjointSet;
import dataStructures.set.DisjointSetDictionary;

public class Main {
    public static void main(String[] args) {
        DisjointSet<String> dj = new DisjointSetDictionary<>();
        String[] pals = { "hola", "a", "todos", "como", "estais", "por", "aqui", "bien" };
        for (String s : pals) {
            dj.add(s);
        }
        System.out.println(dj);
        System.out.println(dj.numElements());
        dj.union("hola", "a");
        System.out.println(dj);
        dj.union("como", "estais");
        System.out.println(dj);
        dj.union("por", "aqui");
        System.out.println(dj);
        dj.union("hola", "bien");
        System.out.println(dj);
        dj.union("estais", "por");
        System.out.println(dj);
        System.out.println(dj.kind("bien"));
        System.out.println(dj.areConnected("hola", "por"));
        System.out.println(dj.areConnected("hola", "adios"));
        System.out.println(dj.areConnected("no", "esta"));

        // Solo alumnos sin evaluacion continua.
        // =====================================
        // Quitar comentarios a las lineas siguientes
        // para probar flatten() y kinds()

        // dj.flatten();
        // System.out.println(dj);
        // System.out.println(dj.kinds());
    }
}
