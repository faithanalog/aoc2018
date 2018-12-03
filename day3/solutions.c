#include <strings.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int id;
    int x;
    int y;
    int w;
    int h;
} Rect;

// parse_rect a rectangle from a line
void parse_rect(char* line, Rect* r) {
    line = line + sizeof("#") - 1;
    r->id = strtol(line, &line, 10);
    line = line + sizeof(" @ ") - 1;
    r->x = strtol(line, &line, 10);
    line = line + sizeof(",") - 1;
    r->y = strtol(line, &line, 10);
    line = line + sizeof(": ") - 1;
    r->w = strtol(line, &line, 10);
    line = line + sizeof("x") - 1;
    r->h = strtol(line, &line, 10);
}

#define FABRIC_DIMENSIONS 1000
int fabric[FABRIC_DIMENSIONS * FABRIC_DIMENSIONS];

void paint_rect(Rect* r) {
    for (int y = r->y; y < r->y + r->h; y++) {
        for (int x = r->x; x < r->x + r->w; x++) {
            fabric[x + y * FABRIC_DIMENSIONS]++;
        }
    }
}

int is_non_overlapping_claim(Rect* r) {
    for (int y = r->y; y < r->y + r->h; y++) {
        for (int x = r->x; x < r->x + r->w; x++) {
            if (fabric[x + y * FABRIC_DIMENSIONS] != 1) {
                return 0;
            }
        }
    }
    return 1;
}

int main() {
    Rect claims[2048];

    char* line = NULL;
    size_t linelen = 0;
    FILE* input_file = fopen("input", "r");
    int num_claims = 0;
    
    while (getline(&line, &linelen, input_file) > 0) {
        if (num_claims == 2048) {
            fprintf(stderr, "Error: more than 2048 claims in input file.\n");
            exit(1);
        }
        parse_rect(line, &claims[num_claims]);
        paint_rect(&claims[num_claims]);
        num_claims++;
    }

    free(line);

    // Part 1
    int num_overlapping_tiles = 0;
    for (int y = 0; y < FABRIC_DIMENSIONS; y++) {
        for (int x = 0; x < FABRIC_DIMENSIONS; x++) {
            if (fabric[x + y * FABRIC_DIMENSIONS] >= 2) {
                num_overlapping_tiles++;
            }
        }
    }
    printf("Part 1: %d\n", num_overlapping_tiles);

    // Part 2
    for (int c = 0; c < num_claims; c++) {
        if (is_non_overlapping_claim(&claims[c])) {
            printf("Part 2: %d\n", claims[c].id);
        }
    }
}
