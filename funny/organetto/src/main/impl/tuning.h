#ifndef	TUNING_H
#define TUNING_H

enum tuning_system {
	TUNING_EQUAL,
	TUNING_PYTHAGOREAN,
	TUNING_CARLOS_ALPHA
};

double frequency_of(enum tuning_system tuning, int note);

#endif

