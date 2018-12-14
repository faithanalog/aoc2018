#include <stdio.h>
#include <errno.h>
#include <stdlib.h>

#define WORLD_SIZE 150

typedef enum {
    DirUp,
    DirLeft,
    DirDown,
    DirRight
} Dir;

typedef enum {
    TurnLeft,
    TurnForward,
    TurnRight
} TurnDir;

typedef struct {
    int x;
    int y;
    Dir dir;
    TurnDir nextTurn;
} Cart;

typedef struct {
    int has_cart_collided;
    int collision_x;
    int collision_y;
    int last_cart_x;
    int last_cart_y;
} Results;

int tracks[WORLD_SIZE * WORLD_SIZE];

Cart* cart_position_registry[WORLD_SIZE * WORLD_SIZE];

Cart carts[50];
int num_carts = 0;

void insert_cart(int x, int y, Dir dir) {
    carts[num_carts] =
        (Cart) {
            .x = x,
            .y = y,
            .dir = dir,
            .nextTurn = TurnLeft
        };
    cart_position_registry[x + y * WORLD_SIZE] = &carts[num_carts];
    num_carts++;
}

void parse_world(char *input) {
    int x = 0;
    int y = 0;
    while (y < WORLD_SIZE) {
        switch (*input) {
        case 0:
            return;
        case '\n':
            x = 0;
            y++;
            input++;
            continue;
        case '<':
            insert_cart(x, y, DirLeft);
            tracks[x + y * WORLD_SIZE] = '-';
            break;
        case '>':
            insert_cart(x, y, DirRight);
            tracks[x + y * WORLD_SIZE] = '-';
            break;
        case '^':
            insert_cart(x, y, DirUp);
            tracks[x + y * WORLD_SIZE] = '|';
            break;
        case 'v':
            insert_cart(x, y, DirDown);
            tracks[x + y * WORLD_SIZE] = '|';
            break;
        default:
            tracks[x + y * WORLD_SIZE] = *input;
            break;
        }
        x++;
        input++;
    }
}

// Step a cart, and return the cart it collides with (if any)
Cart* step_cart(Cart *c) {
    int nx = 0;
    int ny = 0;
    switch (c->dir) {
    case DirLeft:
        nx = c->x - 1;
        ny = c->y;
        break;
    case DirRight:
        nx = c->x + 1;
        ny = c->y;
        break;
    case DirUp:
        nx = c->x;
        ny = c->y - 1;
        break;
    case DirDown:
        nx = c->x;
        ny = c->y + 1;
        break;
    }

    // First check for collision
    if (cart_position_registry[nx + ny * WORLD_SIZE] != NULL) {
        return cart_position_registry[nx + ny * WORLD_SIZE];
    }

    // Move forward
    cart_position_registry[c->x + c->y * WORLD_SIZE] = NULL;
    c->x = nx;
    c->y = ny;
    cart_position_registry[nx + ny * WORLD_SIZE] = c;

    // Figure out the new direction based on the underlying tile
    switch (tracks[nx + ny * WORLD_SIZE]) {
    case '/':
        switch (c->dir) {
        case DirLeft:
            c->dir = DirDown;
            break;
        case DirRight:
            c->dir = DirUp;
            break;
        case DirUp:
            c->dir = DirRight;
            break;
        case DirDown:
            c->dir = DirLeft;
            break;
        }
        break;
    case '\\':
        switch (c->dir) {
        case DirLeft:
            c->dir = DirUp;
            break;
        case DirRight:
            c->dir = DirDown;
            break;
        case DirUp:
            c->dir = DirLeft;
            break;
        case DirDown:
            c->dir = DirRight;
            break;
        }
        break;
    case '+':
        switch (c->nextTurn) {
        case TurnLeft:
            c->dir = (c->dir + 1) % 4;
            c->nextTurn = TurnForward;
            break;
        case TurnForward:
            c->nextTurn = TurnRight;
            break;
        case TurnRight:
            c->dir = (c->dir - 1 + 4) % 4;
            c->nextTurn = TurnLeft;
            break;
        }
    }
    return NULL;
}

// Returns whether true if you should run again.
// Returns false if you're done
int step_all_carts(Results* results) {
    Cart *cart_sequence[50];

    // I'm aware of how silly a method of sorting this is
    int n = 0;
    for (int y = 0; y < WORLD_SIZE; y++) {
        for (int x = 0; x < WORLD_SIZE; x++) {
            if (cart_position_registry[x + y * WORLD_SIZE] != NULL) {
                cart_sequence[n] = cart_position_registry[x + y * WORLD_SIZE];
                n++;
            }
        }
    }

    if (n == 1) {
        results->last_cart_x = cart_sequence[0]->x;
        results->last_cart_y = cart_sequence[0]->y;
        return 0;
    }

    for (int i = 0; i < n; i++) {
        Cart *cart = cart_sequence[i];
        if (cart_position_registry[cart->x + cart->y * WORLD_SIZE] == NULL) {
            // TL;DR if its null thats because it was part of a collision
            // in an earlier loop cycle
            continue;
        }
        Cart *collision = step_cart(cart);
        if (collision != NULL) {
            if (!(results->has_cart_collided)) {
                results->collision_x = collision->x;
                results->collision_y = collision->y;
                results->has_cart_collided = 1;
            }
            cart_position_registry[collision->x + collision->y * WORLD_SIZE] = NULL;
            cart_position_registry[cart->x + cart->y * WORLD_SIZE] = NULL;
        }
    }

    return 1;
}

int main() {
    char read_buffer[(WORLD_SIZE + 1) * WORLD_SIZE];
    FILE *input = fopen("input", "r");

    size_t n = fread(read_buffer, 1, sizeof(read_buffer), input);
    if (n < sizeof(read_buffer)) {
        if (n == 0) {
            perror("Error reading input");
            exit(1);
        }
        read_buffer[n] = 0;
    }

    parse_world(read_buffer);

    Results results = {
        .has_cart_collided = 0,
        .collision_x = 0,
        .collision_y = 0,
        .last_cart_x = 0,
        .last_cart_y = 0
    };
    while (step_all_carts(&results)) {
    }
    printf("Part 1: %d,%d\nPart 2: %d,%d\n",
        results.collision_x,
        results.collision_y,
        results.last_cart_x,
        results.last_cart_y
    );
}
