package weather;
import java.io.File;
import java.net.URL;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.*;
public class Weather{
    public static void updateXML(){
        try{
            URL weatherPage=new URL("http://www.google.com/ig/api?weather=moscow");
            //File weather=new File(weatherPage.getPath());
            Document doc=DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(weatherPage.toURI().toString());
            //doc.getDocumentElement().normalize();
        }catch(Exception ex){
            ex.printStackTrace();
        }
    }
}
