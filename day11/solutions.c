#include <stdio.h>

// Puzzle input
#define SERIAL_NUMBER 4172

#define GRID_SIZE 300

// Caches of the power levels of each cell.
// The transposed version is used for more CPU cache efficiency when
// calculating power vertically
int powerCache[GRID_SIZE * GRID_SIZE];
int powerCacheTransposed[GRID_SIZE * GRID_SIZE];

// Caches of the power-square sums
int gridCache[GRID_SIZE * GRID_SIZE];

int powerLevelCalc(int x, int y) {
    int rackID = x + 10;
    int power = rackID * y;
    power = power + SERIAL_NUMBER;
    power = power * rackID;
    power = (power % 1000) / 100;
    power = power - 5;
    return power;
}

int powerLevel(int x, int y) {
    return powerCache[(x - 1) + (y - 1) * GRID_SIZE];
}

void primeCache() {
    for (int y = 1; y <= GRID_SIZE; y++) {
        for (int x = 1; x <= GRID_SIZE; x++) {
            powerCache[x - 1 + (y - 1) * GRID_SIZE] = powerLevelCalc(x, y);
            gridCache[x - 1 + (y - 1) * GRID_SIZE] = powerLevelCalc(x, y);
            powerCacheTransposed[y - 1 + (x - 1) * GRID_SIZE] = powerLevelCalc(x, y);
        }
    }
}

int updatePowerSquare(int x, int y, int size) {
    int p = gridCache[x - 1 + (y - 1) * GRID_SIZE];

    int *pcache = &powerCache[x - 1 + (y + size - 1 - 1) * GRID_SIZE];
    for (int i = 0; i < size; i++) {
        p += pcache[i];
    }

    pcache = &powerCacheTransposed[y - 1 + (x + size - 1 - 1) * GRID_SIZE];
    for (int i = 0; i < size - 1; i++) {
        p += pcache[i];
    }

    gridCache[x - 1 + (y - 1) * GRID_SIZE] = p;
    return p;
}

int maxPowerSquare(int* gridX, int* gridY, int* power, int size) {
    int increasedMax = 0;
    for (int y = 1; y <= GRID_SIZE - (size - 1); y++) {
        for (int x = 1; x <= GRID_SIZE - (size - 1); x++) {
            int nextPower = updatePowerSquare(x, y, size);
            if (nextPower > *power) {
                *power = nextPower;
                *gridX = x;
                *gridY = y;
                increasedMax = 1;
            }
        }
    }
    return increasedMax;
}

int main() {
    int x = 0;
    int y = 0;
    int maxSize = 0;
    int power = 0;
    primeCache();
    
    // Manually do 2 and 3 to get Part 1
    maxPowerSquare(&x, &y, &power, 2);
    maxPowerSquare(&x, &y, &power, 3);
    printf("Part 1: %d,%d\n", x, y);
    for (int size = 4; size <= GRID_SIZE; size++) {
        if (maxPowerSquare(&x, &y, &power, size)) {
            maxSize = size;
        }
    }
    printf("Part 2: %d,%d,%d\n", x, y, maxSize);
}
