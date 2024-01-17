/*
Do whatever you want with this code
 */

package harius.snowflakes;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.sound.midi.*;

public class Sound
{
    private World world;
    private Synthesizer synth;
    private MidiChannel channel;
    private Instrument bell;
    private static final int[] NOTES = {60, 64, 67, 69, 74, 72, 76, 79, 81, 86, 84, 88, 91, 93, 98};
    
    public Sound(World world) {
        this.world = world;
        try {
            synth = MidiSystem.getSynthesizer();
            synth.open();
            channel = synth.getChannels()[0];
            for(int i = 0; i < synth.getDefaultSoundbank().getInstruments().length; ++i) {
                System.out.print(i + " ");
                System.out.println(synth.getDefaultSoundbank().getInstruments()[i].getName());
            }
            bell = synth.getDefaultSoundbank().getInstruments()[39];
            synth.loadInstrument(bell);
        } catch (MidiUnavailableException ex) {
            Logger.getLogger(Sound.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public void drop(double x) {
        double factor = x / world.getWidth();
        int element = (int) (factor * NOTES.length);
        if(element >= NOTES.length)
            element = NOTES.length - 1;
        if(element < 0)
            element = 0;
        channel.noteOn(NOTES[element], 50);
    }
}
