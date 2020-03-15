package edem_view;
import edem_main.Const;
import java.awt.Color;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.*;
import java.io.File;
import java.util.Random;
import javax.imageio.ImageIO;
import javax.swing.JFrame;
public class ImageFactory{
    private final static double rt=Const.RT;
    private final static Random rand=new Random();
    private static Image makeTransparent(Image src,final int keyColor){
        final int marker=keyColor|0xff000000;
        ImageFilter filter=new RGBImageFilter(){
            @Override
            public int filterRGB(int x, int y, int rgb){
                if(rgb==marker){
                    return 0;
                }
                else{
                    return rgb;
                }
            }
        };
        ImageProducer ip=new FilteredImageSource(src.getSource(),filter);
        return Toolkit.getDefaultToolkit().createImage(ip);
    }
    public static Image fromFile(String filename){
        try{
            File f=new File(filename);
            Image tmp=ImageIO.read(ImageFactory.class.getResourceAsStream("/" + filename));
            tmp=makeTransparent(tmp,0xffffff);
            return tmp;
        }
        catch(Exception e){
            return null;
        }
    }
    public static Image generateRomb(int length,Color base,double noise_amount){
        int w=(int)(length*rt);
        int h=length;
        int pix[]=new int[w*h];
        for(int i=0;i<h;i++){
            for(int j=0;j<w;j++){
                if(Math.abs(i-h/2)<=Math.abs(Math.abs(-j*h/w+h/2)-h/2)){
                    if(rand.nextDouble()<noise_amount){
                        int red=base.getRed();
                        int green=base.getGreen();
                        int blue=base.getBlue();
                        red=getRand(Math.max(0,red-15),Math.min(255,red+15));
                        green=getRand(Math.max(0,green-15),Math.min(255,green+15));
                        blue=getRand(Math.max(0,blue-15),Math.min(255,blue+15));
                        pix[i*w+j]=new Color(red,green,blue).getRGB();
                    }
                    else pix[i*w+j]=base.getRGB();
                }
            }
        }
        Image tmp=new JFrame().createImage(new MemoryImageSource(w,h,pix,0,w));
        return tmp;
    }
    public static Image generateSecondary(int length,Color base,double noise_amount){
        int w=(int)(length*rt);
        int h=length;
        int pix[]=new int[w*h];
        for(int i=0;i<h;i++){
            for(int j=0;j<w;j++){
                if(Math.abs(i-h/2)<=Math.abs(Math.abs(-j*h/w+h/2)-h/2)){}
                else{
                    double dist=(i-h/2)*(i-h/2)+(j-w/2)*(j-w/2);
                    double max=w*h/4;
                    //if(dist/max<0.7&&rand.nextDouble()<noise_amount){
                    if(rand.nextDouble()<noise_amount){
                        pix[i*w+j]=new Color(base.getRed(),base.getGreen(),base.getBlue(),50).getRGB();
                    }
                }
            }
        }
        Image tmp=new JFrame().createImage(new MemoryImageSource(w,h,pix,0,w));
        return tmp;
    }
    private static int getRand(int min,int max){
        return rand.nextInt(max-min)+min;
    }
}
