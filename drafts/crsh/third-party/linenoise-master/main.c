#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include "linenoise.h"

void completion(const char *buf, linenoiseCompletions *lc) {
    if (buf[0] == 'h') {
        linenoiseAddCompletion(lc,"hello");
        linenoiseAddCompletion(lc,"hello there");
    }
}

char *hints(const char *buf, int *color, int *bold) {
    if (!strcasecmp(buf,"hello")) {
        *color = 35;
        *bold = 0;
        return " World";
    }
    return NULL;
}

int main(int argc, char **argv) {
    char *line;
    assert(argc == 2);
    char *fname = argv[1];
    int async = 0;

    FILE *outp = fopen(fname, "w");

    /* Set the completion callback. This will be called every time the
     * user uses the <tab> key. */
    linenoiseSetCompletionCallback(completion);
    linenoiseSetHintsCallback(hints);

    /* Load history from file. The history file is just a plain text file
     * where entries are separated by newlines. */
    linenoiseHistoryLoad("/Users/zahaaar/.crsh-history"); /* Load the history at startup */

    /* Now this is the main loop of the typical linenoise-based application.
     * The call to linenoise() will block as long as the user types something
     * and presses enter.
     *
     * The typed string is returned as a malloc() allocated string by
     * linenoise, so the user needs to free() it. */

    do {
        if (!async) {
            line = linenoise("> ");
            if (line == NULL) break;
        } else {
            /* Asynchronous mode using the multiplexing API: wait for
             * data on stdin, and simulate async data coming from some source
             * using the select(2) timeout. */
            struct linenoiseState ls;
            char buf[1024];
            linenoiseEditStart(&ls,-1,-1,buf,sizeof(buf),"hello> ");
            while(1) {
		fd_set readfds;
		struct timeval tv;
		int retval;

		FD_ZERO(&readfds);
		FD_SET(ls.ifd, &readfds);
		tv.tv_sec = 1; // 1 sec timeout
		tv.tv_usec = 0;

		retval = select(ls.ifd+1, &readfds, NULL, NULL, &tv);
		if (retval == -1) {
		    perror("select()");
                    exit(1);
		} else if (retval) {
		    line = linenoiseEditFeed(&ls);
                    /* A NULL return means: line editing is continuing.
                     * Otherwise the user hit enter or stopped editing
                     * (CTRL+C/D). */
                    if (line != linenoiseEditMore) break;
		} else {
		    // Timeout occurred
                    static int counter = 0;
                    linenoiseHide(&ls);
		    printf("Async output %d.\n", counter++);
                    linenoiseShow(&ls);
		}
            }
            linenoiseEditStop(&ls);
            if (line == NULL) exit(0); /* Ctrl+D/C. */
        }

        /* Do something with the string. */
        if (line[0] != '\0' && line[0] != '/') {
            fprintf(outp, "%s\n", line);
            linenoiseHistoryAdd(line); /* Add to the history. */
            linenoiseHistorySave("/Users/zahaaar/.crsh-history"); /* Save the history on disk. */
        } else if (!strncmp(line,"/historylen",11)) {
            /* The "/historylen" command will change the history len. */
            int len = atoi(line+11);
            linenoiseHistorySetMaxLen(len);
        } else if (!strncmp(line, "/mask", 5)) {
            linenoiseMaskModeEnable();
        } else if (!strncmp(line, "/unmask", 7)) {
            linenoiseMaskModeDisable();
        } else if (line[0] == '/') {
            printf("Unreconized command: %s\n", line);
        }
        free(line);
    } while (0);
    fclose(outp);
    return 0;
}