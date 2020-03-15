/**
 * Project: GotTheRhythm
 * Date: 13.12.13
 */
public class App {
    public static void main(String[] args) {
        Sound s = new Sound();
        Rhythm base = new Rhythm(1, 2, 1, 2, 2);
        Rhythm chunk = new Rhythm(2, 1, 1);
        Rhythm complex1 = base.multiply(chunk);
        Rhythm complex2 = chunk.multiply(base);
        s.loop(complex1, 5000, 10);
        s.close();
    }
}
