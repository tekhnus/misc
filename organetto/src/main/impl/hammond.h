#ifndef HAMMOND_H
#define HAMMOND_H

#include "common.h"
#include "synth.h"

#define HAMMOND_HARMONIC_COUNT 9

struct hammond_params {
	sample_t *amplitudes;
	sample_t *decay_powers;
	/* double rotary_frequency;
	double rotary_depth; */
};

extern struct hammond_params organ_hammond_params;

void buffer_add_hammond(size_t buffer_index,
		jack_nframes_t sample_rate,
		jack_nframes_t frame_offset,
		struct note_state *state,
		unsigned char note, sample_t amplitude,
		struct hammond_params *instrument);

#endif

