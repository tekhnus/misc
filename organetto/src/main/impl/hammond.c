#include "hammond.h"
#include "tuning.h"
#include "controls.h"

static unsigned char base_note = 69;
static double base_note_frequency = 440.0;
static double hammond_harmonics[HAMMOND_HARMONIC_COUNT] =
	{0.5, 1.5, 0.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0};

static sample_t organ_hammond_amplitudes[HAMMOND_HARMONIC_COUNT] =
	{8.0, 8.0, 8.0, 6.0, 4.0, 3.0, 2.0, 0.0, 0.0};
static sample_t organ_hammond_decay_powers[HAMMOND_HARMONIC_COUNT] =
	{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
struct hammond_params organ_hammond_params = {organ_hammond_amplitudes,
					      organ_hammond_decay_powers,
					      /* 8.0, 0.05 */};

void buffer_add_hammond(size_t buffer_index,
		jack_nframes_t sample_rate,
		jack_nframes_t frame_offset,
		struct note_state *state,
		unsigned char note, sample_t amplitude,
		struct hammond_params *instrument)
{
	size_t harmonic;
	double note_freq, harmonic_freq;
	if (!state->is_on)
		return;
	note_freq = base_note_frequency *
			frequency_of(organetto_tuning, note - base_note);
	for (harmonic = 0; harmonic < HAMMOND_HARMONIC_COUNT; ++harmonic) {
		harmonic_freq = note_freq * hammond_harmonics[harmonic];
		buffer_clean(BUFFER_HAMMOND_WAVE);
		buffer_add_sine(BUFFER_HAMMOND_WAVE, sample_rate,
			state->duration,
			harmonic_freq, amplitude * instrument->amplitudes[harmonic]);
		buffer_clean(BUFFER_HAMMOND_ENVELOPE);
		buffer_add_reverse_poly(BUFFER_HAMMOND_ENVELOPE, sample_rate,
				state->duration,
				instrument->decay_powers[harmonic], 1.0);
		buffer_multiply(BUFFER_HAMMOND_WAVE, BUFFER_HAMMOND_ENVELOPE);
		buffer_add(buffer_index, BUFFER_HAMMOND_WAVE);
	}
	/* buffer_clean(BUFFER_HAMMOND_ROTARY);
	buffer_add_constant(BUFFER_HAMMOND_ROTARY, 1.0);
	buffer_add_sine(BUFFER_HAMMOND_ROTARY, sample_rate, frame_offset,
			instrument->rotary_frequency, instrument->rotary_depth);
	buffer_multiply(buffer_index, BUFFER_HAMMOND_ROTARY); */
}

