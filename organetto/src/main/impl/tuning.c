#include <math.h>

#include "tuning.h"

static double equal_frequency(int note);
static double pythagorean_frequency(int note);
static double carlos_alpha_frequency(int note);

static double const pythagorean_multipliers[] = {
	1.0,
	256.0/243,
	9.0/8,
	32.0/27,
	81.0/64,
	4.0/3,
	729.0/512,
	3.0/2,
	128.0/81,
	27.0/16,
	16.0/9,
	243.0/128
};

double frequency_of(enum tuning_system tuning, int note)
{
	switch (tuning) {
	case TUNING_EQUAL:
		return equal_frequency(note);
	case TUNING_PYTHAGOREAN:
		return pythagorean_frequency(note);
	case TUNING_CARLOS_ALPHA:
	default:
		return carlos_alpha_frequency(note);
	}
}

static double pythagorean_frequency(int note)
{
	/* TODO Don't be lazy, avoid unnecessary recursion. */
	if (note >= 12) {
		return 2 * pythagorean_frequency(note - 12);
	}
	if (note < 0) {
		return 0.5 * pythagorean_frequency(note + 12);
	}
	return pythagorean_multipliers[note];
}

static double equal_frequency(int note)
{
	return pow(2, note / 12.0);
}

static double carlos_alpha_frequency(int note)
{
	return pow(2, note / 15.3915);
}

