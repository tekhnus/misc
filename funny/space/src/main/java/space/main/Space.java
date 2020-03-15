package space.main;

import java.awt.Color;
import java.util.Random;
import java.util.Stack;
import javax.swing.JApplet;
import javax.swing.JFrame;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;
import space.math.Vec2;
import space.universe.Body;
import space.universe.Universe;
import space.universe.UniverseView;

/**
 * The main class of application
 */

public class Space extends JApplet implements Runnable
{
    private Universe u;
    private UniverseView view;
    private int w, h;
    private boolean running;
    private Thread th;
    private static String defaultFile = "/map.xml";//"res/planets.res";
    
    /**
     * Load universe data from file
     * @param filename Path to file
     */
    
    private void loadUniverse(String filename)
    {
        u = new Universe();
        try
        {
            SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
            DefaultHandler handler = new DefaultHandler()
            {
                Stack<Double> rads = new Stack<Double>();
                Stack<Double> velos = new Stack<Double>();
                
                @Override
                public void startElement(String uri, String localName, String qName, Attributes attributes)
                {
                    if(qName.equals("planet"))
                    {
                        if(rads.empty())
                            rads.push(0.0);
                        if(velos.empty())
                            velos.push(0.0);
                        String name = attributes.getValue("name");
                        String color = attributes.getValue("color");
                        String rs = attributes.getValue("radius");
                        String ms = attributes.getValue("mass");
                        String os = attributes.getValue("orbit");
                        String vs = attributes.getValue("speed");
                        //String sk = attributes.getValue("skip");
                        double r = Double.parseDouble(rs);
                        double m = Double.parseDouble(ms);
                        double o = (os != null) ? Double.parseDouble(os) : 0;
                        rads.push(rads.peek() + o);
                        double v = (vs != null) ? Double.parseDouble(vs) : 0;
                        velos.push(velos.peek() + v);
                        //boolean skb = ((sk == null) || sk.equals("false"));
                        u.addBody(new Body(name, new Vec2(rads.peek(), 0), new Vec2(0, velos.peek()), r, m, Color.decode(color), true, 0.0));
                    }
                    else if(qName.equals("rings"))
                    {
                        String os = attributes.getValue("orbit");
                        String vs = attributes.getValue("speed");
                        String ns = attributes.getValue("number");
                        String ms = attributes.getValue("minscale");
                        double o = Double.parseDouble(os);
                        double v = Double.parseDouble(vs);
                        double msd = ms != null ? Double.parseDouble(ms) : 0.0;
                        int n = Integer.parseInt(ns);
                        Random r = new Random();
                        double a;
                        double curR;
                        for(int i = 0; i < n; i++)
                        {
                            a = r.nextDouble() * 2 * Math.PI;
                            curR = (r.nextDouble() / 10 + 1) * o;
                            u.addBody(new Body("ring "+ i, new Vec2(rads.peek() + curR * Math.cos(a), curR * Math.sin(a)), new Vec2(-v * Math.sin(a), velos.peek() + v * Math.cos(a)), 0.1, 1, new Color(150,150,150), false, msd));
                        }
                    }
                }
                
                @Override
                public void endElement(String uri, String localName, String qName)
                {
                    if(qName.equals("planet"))
                    {
                        rads.pop();
                        velos.pop();
                    }
                }
            };
            parser.parse(Space.class.getResourceAsStream(filename), handler);
            u.dump();
        }
        catch(Exception ex)
        {
            System.err.println("Problems with parser");
            ex.printStackTrace();
        }
    };
    
    private void loadView()
    {
        w = 1024;
        h = 640;
        view = new UniverseView(u, w, h);
        view.fixAt(0);
    }
    
    private void startSimulation()
    {
        running = true;
        th = new Thread(this);
        th.start();
    }
    
    private void desktopStart()
    {
        JFrame fr = new JFrame("Space");
        fr.setSize(w, h);
        fr.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        fr.setVisible(true);
        fr.add(view);
        fr.addKeyListener(view);
        startSimulation();
    }
    
    /**
     * Starting point for desktop application 
     * @param args Arguments specified
     * @throws InterruptedException 
     */
    
    public static void main(String[] args) throws InterruptedException
    {
        Space space = new Space();
        space.desktopStart();
    }
    
    public Space()
    {
        loadUniverse(defaultFile);
        loadView();
    }
    
    /**
     * Starting point for applet
     */
    
    @Override
    public void init()
    {
        loadUniverse(defaultFile);
        loadView();
        add(view);
        addKeyListener(view);
        startSimulation();
    }

    @Override
    public void run()
    {
        while(running)
        {
            for(int i = 0; i < 10; i++)
                u.simulate();
            /*try{
                Thread.sleep(1);
            }
            catch(InterruptedException ex)
            {
                System.err.println("Threading error");
                ex.printStackTrace();
            }*/
        }
    }
    
    @Override
    public void destroy()
    {
        running = false;
        th = null;
    }
        
}
