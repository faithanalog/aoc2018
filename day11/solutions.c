#define SERIAL_NUMBER 4172

#define GRID_SIZE 300

#include <stdio.h>


int powerCache[GRID_SIZE * GRID_SIZE];

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
    if (x < 1 || y < 1 || x > GRID_SIZE || y > GRID_SIZE) {
        return 0;
    }
    return powerCache[(x - 1) + (y - 1) * GRID_SIZE];
}

void primeCache() {
    for (int y = 1; y <= GRID_SIZE; y++) {
        for (int x = 1; x <= GRID_SIZE; x++) {
            powerCache[x - 1 + (y - 1) * GRID_SIZE] = powerLevelCalc(x, y);
            gridCache[x - 1 + (y - 1) * GRID_SIZE] = powerLevelCalc(x, y);
        }
    }
}

int powerSquare(int x, int y, int size) {
    int p = gridCache[x - 1 + (y - 1) * GRID_SIZE];

    for (int xx = x; xx < x + size; xx++) {
        p += powerLevel(xx, y + size - 1);
    }
    for (int yy = y; yy < y + size - 1; yy++) {
        p += powerLevel(x + size - 1, yy);
    }

    gridCache[x - 1 + (y - 1) * GRID_SIZE] = p;
    return p;
}

void maxPowerSquare(int* gridX, int* gridY, int* power, int size) {
    int p = *power;
    int gx = *gridX;
    int gy = *gridY;
    for (int y = 1; y <= GRID_SIZE - (size - 1); y++) {
        for (int x = 1; x <= GRID_SIZE - (size - 1); x++) {
            int nextPower = powerSquare(x, y, size);
            if (nextPower > p) {
                p = nextPower;
                gx = x;
                gy = y;
            }
        }
    }
    *gridX = gx;
    *gridY = gy;
    *power = p;
}

void maxPowerSquareForever(int* maxX, int* maxY, int* maxSize) {
    int power = 0;
    for (int size = 2; size <= GRID_SIZE; size++) {
        int p = 0;
        int x = 1;
        int y = 1;
        maxPowerSquare(&x, &y, &p, size);
        if (size == 3) {
            printf("Part 1: %d,%d\n", x, y);
        }
        if (p > power) {
            power = p;
            *maxSize= size;
            *maxX = x;
            *maxY = y;
        }
    }
}

int main() {
    int x = 0;
    int y = 0;
    int size = 0;
    primeCache();
    maxPowerSquareForever(&x, &y, &size);
    printf("Part 2: %d,%d,%d\n", x, y, size);
}
