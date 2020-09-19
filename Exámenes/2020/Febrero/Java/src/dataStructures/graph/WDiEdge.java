package dataStructures.graph;

public class WDiEdge<V,W> { // Weighted edge from src to dst with weight weight
    private V src, dst;
    private W weight;
	
    public WDiEdge(V s, W w, V d) {
        src = s;
        dst = d;
        weight = w;
    }

    public V getSrc() {
        return src;
    }

    public V getDst() {
        return dst;
    }

    public W getWeight() {
        return weight;
    }

    @Override
    public String toString() {
        return src + "-" + weight+ "->" + dst;
    }

    @Override
    public boolean equals(Object obj) {
        if(!(obj instanceof WDiEdge<?,?>))
            return false;
        else {
            WDiEdge<?,?> that = (WDiEdge<?,?>) obj;
            return this.src.equals(that.src) &&
                    this.dst.equals(that.dst) &&
                    this.weight.equals(that.weight);
        }
    }

    @Override
    public int hashCode(){
        int hash = 17;
        hash = 31*hash + src.hashCode();
        hash = 31*hash + dst.hashCode();
        hash = 31*hash + weight.hashCode();
        return hash;
    }
}