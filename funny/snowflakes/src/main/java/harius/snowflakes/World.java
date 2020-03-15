/*
Do whatever you want with this code
 */

package harius.snowflakes;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import mikera.vectorz.*;

public class World
{
    private double width;
    private double height;
    private Vector2 gravity;
    private List<Flake> flakes;
    double time;
    
    public World(double width, double height, Vector2 gravity) {
        this.width = width;
        this.height = height;
        this.gravity = gravity;
        flakes = new LinkedList<Flake>();
        time = 0.0;
    }
    
    public void tick(double ms) {
        Iterator<Flake> it = this.flakes.iterator();
        while(it.hasNext()) {
            Flake flake = it.next();
            flake.tick(ms);
            if(flake.getLocation().y < -this.height/10.0) {
                it.remove();
            }
        }
        time += ms;
    }
    
    public Vector2 forceAt(Vector2 location, double mass, Vector2 speed) {
        Vector2 force = Vector2.of(0.0, 0.0);
        Vector2 grav = this.gravityAt(mass);
        force.add(grav);
        Vector2 resist = this.resistAt(speed);
        force.add(resist);
        Vector2 wind = this.windAt(location);
        force.add(wind);
        return force;
    }
    
    private Vector2 gravityAt(double mass) {
        Vector2 force = this.gravity.clone();
        force.multiply(mass);
        return force;
    }
    
    private Vector2 resistAt(Vector2 speed) {
        Vector2 force = speed.clone();
        force.multiply(-0.2);
        return force;
    }
    
    private Vector2 windAt(Vector2 location) {
        double x = 0.001 * Math.sin(0.2 * location.y);
        double y = 0.001 * Math.cos(0.3 * location.y);
        return Vector2.of(x, y);
    }
    
    public List<Flake> getFlakes() {
        return this.flakes;
    }
    
    public void addFlake(Flake flake) {
        this.flakes.add(flake);
    }
    
    public double getHeight() {
        return height;
    }

    public double getWidth() {
        return this.width;
    }
}
