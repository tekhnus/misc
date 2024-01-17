#ifndef MIDIUTILS_H
#define MIDIUTILS_H

#include <stddef.h>

#define MASK_STATUS 0x80

#define MASK_CHANNEL 0x0F

#define MASK_TYPE 0x70
#define TYPE_NOTE_ON 0x10
#define TYPE_NOTE_OFF 0x00

typedef unsigned char byte_t;

struct midi_data {
	byte_t *bytes;
	size_t size;
};

struct midi_event {
	byte_t type;
	byte_t channel;
	byte_t pitch;
};	

int midi_is_status_byte(byte_t b);
byte_t midi_get_event_type(byte_t status);
int midi_interpret_data(struct midi_data *data, struct midi_event *event);

#endif

