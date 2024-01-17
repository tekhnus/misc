#ifndef SYNTH_H
#define SYNTH_H

#include <jack/jack.h>

#include "common.h"

struct note_state {
	int is_on;
	jack_nframes_t duration;
};

#define BUFFER_COUNT 30
extern sample_t *buffers[BUFFER_COUNT];
extern jack_nframes_t buffer_size;

enum buffer_purpose {
	BUFFER_OUTPUT,
	BUFFER_HAMMOND_WAVE,
	BUFFER_HAMMOND_ENVELOPE,
	BUFFER_HAMMOND_ROTARY
};

/* This should be called before each iteration of process callback
 * in case chunk size changes.
 */
void synth_set_buffer_size(jack_nframes_t size);

void buffer_clean(size_t buffer_index);

void buffer_add(size_t dest_index, size_t src_index);

void buffer_multiply(size_t dest_index, size_t src_index);

void buffer_output_to(sample_t *dest, size_t src_index);

void buffer_add_constant(size_t buffer_index, sample_t value);

void buffer_add_sine(size_t buffer_index,
		jack_nframes_t sample_rate, int64_t offset,
		double frequency, sample_t amplitude);

void buffer_add_reverse_poly(size_t buffer_index,
		jack_nframes_t sample_rate, int64_t offset,
		double power, double decay_speed);

#endif

