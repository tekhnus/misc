package weather;
import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.text.*;
import javafx.util.Math;
import javafx.animation.*;
import javafx.scene.paint.*;
import javafx.scene.effect.*;
import javafx.scene.shape.*;
Weather.updateXML();
def mornColor=Color.DODGERBLUE;
def dayColor=Color.SKYBLUE;
def dawnColor=Color.GOLDENROD;
def nightColor=Color.DARKBLUE;
def w=800;
def h=400;
def r=h/4;
var time=0.0;
var sun=Circle{
    centerX:w/2
    centerY:bind 5*h/8-5*h/8*Math.cos(2*Math.PI*time/12.0);
    radius:r
    fill:bind if(time>=6 and time<=18)Color.ORANGE else Color.WHITESMOKE;
    effect:GaussianBlur{}
}
var cycle=Timeline{
    keyFrames:KeyFrame{
        time:10s
        values:[time=>24.0]
    }
    repeatCount:Timeline.INDEFINITE
}
var clock=Text{
    font:Font{
        size:40
    }
    content:bind "{time as Integer}:00"
    x:10
    y:50
    fill:Color.WHITE
}
var sky=Rectangle{
    width:w
    height:h
    fill:bind
    if(time>=0 and time<=6)(nightColor.ofTheWay(mornColor,time/6.0) as Color)
    else if(time>=6 and time<=12)(mornColor.ofTheWay(dayColor,(time-6)/6.0) as Color)
    else if(time>=12 and time<=18)(dayColor.ofTheWay(dawnColor,(time-12)/6.0) as Color)
    else (dawnColor.ofTheWay(nightColor,(time-18)/6.0) as Color)
}
Stage{
    title:"Weather"
    scene:Scene{
        width:w
        height:h
        content:bind [
            sky,sun,clock
        ]
    }
}
cycle.play();