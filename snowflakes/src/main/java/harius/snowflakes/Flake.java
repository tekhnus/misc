/*
Do whatever you want with this code
 */

package harius.snowflakes;

import mikera.vectorz.*;

public class Flake
{
    private World world;
    private Sound sound;
    private Vector2 speed;
    private Vector2 location;
    private double mass;
    private boolean dropped = false;
    
    public Flake(World world, Sound sound, Vector2 initialLocation, double mass) {
        this.world = world;
        this.sound = sound;
        this.location = initialLocation;
        this.speed = Vector2.of(0.0, 0.0);
        this.mass = mass;
    }
    
    public void tick(double ms) {
        Vector2 acc = world.forceAt(location, mass, speed);
        acc.divide(mass);
        acc.multiply(ms * 0.001);
        this.speed.add(acc);
        this.location.add(speed);
        if(this.location.y < 1 && !this.dropped) {
            this.sound.drop(this.location.x);
            this.dropped = true;
        }
    }
    
    public Vector2 getLocation() {
        return this.location;
    }
    
    @Override
    public String toString() {
        return "(" + this.location.x + "; " + this.location.y + ")";
    }

    public double getMass() {
        return this.mass;
    }
}
