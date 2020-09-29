#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "synth.h"

#ifndef M_PI
#define M_PI 3.14159265358979
#endif

sample_t *buffers[BUFFER_COUNT];
jack_nframes_t buffer_size = 0;

void synth_set_buffer_size(jack_nframes_t size)
{
	size_t b;
	buffer_size = size;
	for (b = 0; b < BUFFER_COUNT; ++b) {
		buffers[b] =
			realloc(buffers[b], buffer_size * sizeof(sample_t));
	}
}

void buffer_clean(size_t buffer_index)
{
	memset(buffers[buffer_index], 0, buffer_size * sizeof(sample_t));
}

void buffer_add(size_t dest_index, size_t src_index)
{
	jack_nframes_t frame;
	for (frame = 0; frame < buffer_size; ++frame) {
		buffers[dest_index][frame] += buffers[src_index][frame];
	}
}

void buffer_multiply(size_t dest_index, size_t src_index)
{
	jack_nframes_t frame;
	for (frame = 0; frame < buffer_size; ++frame) {
		buffers[dest_index][frame] *= buffers[src_index][frame];
	}
}

void buffer_output_to(sample_t *dest, size_t src_index)
{
	memcpy(dest, buffers[src_index], buffer_size * sizeof(sample_t));
}

void buffer_add_constant(size_t buffer_index, sample_t value)
{
	jack_nframes_t frame;
	for (frame = 0; frame < buffer_size; ++frame) {
		buffers[buffer_index][frame] += value;
	}
}

void buffer_add_sine(size_t buffer_index,
		jack_nframes_t sample_rate, int64_t offset,
		double frequency, sample_t amplitude)
{
	int64_t frame;
	for (frame = 0; frame < buffer_size; ++frame) {
		double frame_time = ((double)offset + frame) / sample_rate;
		double phase = 2 * M_PI * frequency * frame_time;
		buffers[buffer_index][frame] += amplitude * sin(phase);
	}
}

void buffer_add_reverse_poly(size_t buffer_index,
		jack_nframes_t sample_rate, int64_t offset,
		double power, double decay_speed)
{
	int64_t frame = -offset;
	if (frame < 0)
		frame = 0;
	for (; frame < buffer_size; ++frame) {
		double frame_time = ((double)offset + frame) / sample_rate;
		buffers[buffer_index][frame] += 1.0 /
				pow(decay_speed * frame_time + 1, power);
	}
}

