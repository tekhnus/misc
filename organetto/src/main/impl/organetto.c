#include <stdio.h>
#include <limits.h>

#include <jack/jack.h>
#include <jack/midiport.h>

#include "organetto.h"
#include "synth.h"
#include "tuning.h"
#include "controls.h"
#include "midiutils.h"

static jack_client_t *client;
static jack_port_t *keyboard_port;
static jack_port_t *output_port;

static jack_nframes_t sample_rate = 44100;
static jack_nframes_t frame_offset = 0;

#define NOTE_COUNT UCHAR_MAX
static struct note_state note_states[NOTE_COUNT];

static int convert_midi_struct(jack_midi_event_t *src, struct midi_event *dest)
{
	struct midi_data raw;
	raw.size = src->size;
	raw.bytes = src->buffer;
	return midi_interpret_data(&raw, dest);	
}

static void process_midi_event(jack_midi_event_t *raw_event)
{
	struct midi_event event;
	if (convert_midi_struct(raw_event, &event))
		return;
	switch (event.type) {
	case TYPE_NOTE_ON:
		note_states[event.pitch].is_on = 1;
		note_states[event.pitch].duration = -raw_event->time;
		break;
	case TYPE_NOTE_OFF:
		note_states[event.pitch].is_on = 0;
		note_states[event.pitch].duration = -raw_event->time;
		break;
	}
}

static int process_input(jack_nframes_t nframes, void *arg)
{
	void *in_buf = jack_port_get_buffer(keyboard_port, nframes);
	uint32_t n_events = jack_midi_get_event_count(in_buf);	
	uint32_t i;
	jack_midi_event_t event;
	for (i = 0; i < n_events; ++i) {
		jack_midi_event_get(&event, in_buf, i);
		process_midi_event(&event);
	}
	return 0;
}

static int process_output(jack_nframes_t nframes, void *arg)
{
	unsigned char note;
	sample_t *out_buf = (sample_t *)
		jack_port_get_buffer(output_port, nframes);
	synth_set_buffer_size(nframes);
	buffer_clean(BUFFER_OUTPUT);
	for (note = 0; note < NOTE_COUNT; ++note) {
		buffer_add_hammond(BUFFER_OUTPUT, sample_rate, frame_offset,
			&note_states[note],
			note, 0.005,
			organetto_hammond_params);
		note_states[note].duration += nframes;
	}
	buffer_output_to(out_buf, BUFFER_OUTPUT);
	return 0;
}

static int process(jack_nframes_t nframes, void *arg)
{
	process_input(nframes, arg);
	process_output(nframes, arg);
	frame_offset += nframes;
	return 0;
}

static int srate(jack_nframes_t nframes, void *arg)
{
	sample_rate = nframes;
	return 0;
}

static void error(const char *desc)
{
	fprintf(stderr, "JACK: %s\n", desc);
}

static void jack_shutdown(void *arg)
{
	fprintf(stderr, "JACK server shuts down the client\n");
	exit(1);
}

int auto_connect_input(void)
{
	int status = 0;
	const char **ports = jack_get_ports(client, NULL,
			JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput);
	if (!ports || !ports[0]) {
		status = -1;
		fprintf(stderr, "No MIDI keyboard was found\n");
	} else if (jack_connect(client,
				ports[0], jack_port_name(keyboard_port))) {
		status = -1;
		fprintf(stderr, "Failed to connect to MIDI keyboard\n");
	} else {
		fprintf(stderr, "Connected to MIDI keyboard, port %s\n", 
				ports[0]);
	}
	if (ports)
		jack_free(ports);
	return status;
}

int auto_connect_output(void)
{
	int status = 0;
	const char **ports = jack_get_ports(client, NULL, NULL,
			JackPortIsInput | JackPortIsPhysical);
	if (!ports || !ports[0] || !ports[1]) {
		status = -1;
		fprintf(stderr, "No suitable audio output port was found\n");
	} else {
		int connect_left = jack_connect(client,
				jack_port_name(output_port), ports[0]);
		int connect_right = jack_connect(client,
				jack_port_name(output_port), ports[1]); 
		if (connect_left || connect_right) {
			status = -1;
			fprintf(stderr, "Failed to connect to audio output\n");
		} else {
			fprintf(stderr,
				"Connected to audio output ports %s and %s\n",
				ports[0], ports[1]);
		}
	}
	if (ports)
		jack_free(ports); 
	return status;
}

int setup_organetto(void)
{
	jack_set_error_function(error);
	if ((client = jack_client_open("organetto", JackNoStartServer, NULL)) == NULL) {
		fprintf(stderr, "Couldn't connect to JACK server\n");
		return -1;
	}
	jack_set_process_callback(client, process, 0);
	jack_set_sample_rate_callback(client, srate, 0);
	jack_on_shutdown(client, jack_shutdown, 0);
	keyboard_port = jack_port_register(client, "keyboard",
			JACK_DEFAULT_MIDI_TYPE, JackPortIsInput, 0);
	output_port = jack_port_register(client, "output",
			JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
	if (!keyboard_port || !output_port) {
		fprintf(stderr, "Couldn't open JACK ports\n");
		return -1;
	}
	return 0;
}

int activate_organetto(void)
{
	if (jack_activate(client)) {
		fprintf(stderr, "Cannot activate client");
		return -1;
	}	
	return 0;
}

