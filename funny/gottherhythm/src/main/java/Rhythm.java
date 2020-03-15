import java.util.ArrayList;
import java.util.List;

/**
 * Project: GotTheRhythm
 * Date: 13.12.13
 */
public class Rhythm {
    private List<Integer> durations;

    public Rhythm(List<Integer> durations) {
        this.durations = durations;
    }

    public Rhythm(int... durations) {
        this.durations = new ArrayList<Integer>();
        for (int d : durations) {
            this.durations.add(d);
        }
    }

    public List<Integer> getDurations() {
        return durations;
    }

    public Rhythm multiply(Rhythm r) {
        List<Integer> prodDur = new ArrayList<Integer>();
        for (int bigD : r.getDurations()) {
            for (int littleD : this.getDurations()) {
                prodDur.add(bigD * littleD);
            }
        }
        return new Rhythm(prodDur);
    }

    public Rhythm pow(int k) {
        if (k == 1) {
            return this;
        }
        return this.multiply(this.pow(k - 1));
    }
}
