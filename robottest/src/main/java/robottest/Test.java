package robottest;

import java.awt.Robot;
import java.awt.event.InputEvent;
import java.util.Random;

public class Test{
    public static Robot r;
    public static Random rand;
    public static int x0=880;
    public static int y0=70;
    public static int sz=27;
    public static double rt=Math.sqrt(3.0);
    public static void choose(){
        r.mouseMove(x0+sz*(1+rand.nextInt(9)),y0+sz);
        r.mousePress(InputEvent.BUTTON1_MASK);
        r.mouseRelease(InputEvent.BUTTON1_MASK);
    }
    public static void ln(int x1,int y1,int x2,int y2){
        r.mouseMove(x1,y1);
        r.mousePress(InputEvent.BUTTON1_MASK);
        r.mouseMove(x2,y2);
        r.mouseRelease(InputEvent.BUTTON1_MASK);
    }
    public static void tr(double x0,double y0,double len){
        if(len<=10)return;
        choose();
        ln((int)x0,(int)y0,(int)(x0-len/2),(int)(y0+len*rt/2.0));
        ln((int)(x0-len/2),(int)(y0+len*rt/2.0),(int)(x0+len/2),(int)(y0+len*rt/2.0));
        ln((int)(x0+len/2),(int)(y0+len*rt/2.0),(int)x0,(int)y0);
        tr(x0,y0,len/2);
        tr(x0-len/4,y0+(int)(len*rt/4.0),len/2);
        tr(x0+len/4,y0+(int)(len*rt/4.0),len/2);
    }
    public static void main(String[] args) throws Exception{
        rand=new Random();
        r=new Robot();
        r.setAutoDelay(3);
        r.delay(3000);

        tr(500,200,800);
    }
}
