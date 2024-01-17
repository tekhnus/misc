#include "midiutils.h"

int midi_is_status_byte(byte_t b)
{
	return b & MASK_STATUS;
}

byte_t midi_event_get_type(byte_t status)
{
	return status & MASK_TYPE;
}

int midi_interpret_data(struct midi_data *data, struct midi_event *event)
{
	byte_t status, type;
	if (data->size == 0) {
		return -1;
	}
	status = data->bytes[0];
	if (!midi_is_status_byte(status)) {
		return -1;
	}
	event->channel = status & MASK_CHANNEL;
	type = status & MASK_TYPE;
	switch (type) {
	case TYPE_NOTE_ON:
	case TYPE_NOTE_OFF:
		event->type = type;
		event->pitch = data->bytes[1];
		break;
	default:
		return -1;
	}
	return 0;
}

