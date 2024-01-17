#include "impl/organetto.h"

int main(int argc, char *argv[])
{
	if (setup_organetto()) {
		return -1;
	}
	if (activate_organetto()) {
		return -1;
	}
	auto_connect_input();
	auto_connect_output();
	for(;;);
	return 0;
}

