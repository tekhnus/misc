import javax.sound.midi.*;

 /* Project: GotTheRhythm
 * Date: 13.12.13
 */

public class Sound
{
    private Synthesizer synth;
    private MidiChannel channel;
    private Instrument drum;
    private static final int NOTE = 69;

    public Sound() {
        try {
            synth = MidiSystem.getSynthesizer();
            synth.open();
            channel = synth.getChannels()[0];
            drum = synth.getDefaultSoundbank().getInstruments()[115];
            /* for(int i = 0; i < synth.getDefaultSoundbank().getInstruments().length; ++i) {
                System.out.print(i + " ");
                System.out.println(synth.getDefaultSoundbank().getInstruments()[i].getName());
            } */
            channel.programChange(drum.getPatch().getProgram());
        } catch (MidiUnavailableException ex) {
            System.err.println("MIDI is unavailable");
        }
        channel.noteOn(NOTE, 50);
    }

    public void play(Rhythm rhythm, int period) {
        int time = 0;
        for (int dur : rhythm.getDurations()) {
            time += dur;
        }
        int noteTime = period / time;
        for (int dur : rhythm.getDurations()) {
            channel.noteOn(NOTE, 50);
            this.sleep(dur * noteTime);
            channel.noteOff(NOTE);
        }
    }

    public void loop(Rhythm rhythm, int period, int times) {
        for (int i = 0; i < times; ++i) {
            this.play(rhythm, period);
        }
    }

    private void sleep(int milliseconds) {
        try {
            Thread.sleep(milliseconds);
        } catch (InterruptedException e) {}
    }

    public void close() {
        synth.close();
    }
}


