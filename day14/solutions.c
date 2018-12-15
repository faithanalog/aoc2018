#include <stdlib.h>
#include <stdio.h>

#define INPUT 430971
#define INPUT_ARR { 4, 3, 0, 9, 7, 1 }

int* recipes;
int num_recipes;

int elf_1_slot = 0;
int elf_2_slot = 1;

void step_elves() {
    int new_recipe_digits = recipes[elf_1_slot] + recipes[elf_2_slot];
    int recipe_0 = new_recipe_digits / 10;
    int recipe_1 = new_recipe_digits % 10;
    if (recipe_0 > 0) {
        recipes[num_recipes] = recipe_0;
        num_recipes++;
    }
    recipes[num_recipes] = recipe_1;
    num_recipes++;
    
    elf_1_slot = (elf_1_slot + 1 + recipes[elf_1_slot]) % num_recipes;
    elf_2_slot = (elf_2_slot + 1 + recipes[elf_2_slot]) % num_recipes;
}

int main() {
    recipes = malloc(sizeof(*recipes) * INPUT * 100);
    
    recipes[0] = 3;
    recipes[1] = 7;
    num_recipes = 2;

    while (num_recipes < INPUT * 100) {
        step_elves();
    }

    printf("Part 1: ");
    for (int i = 0; i < 10; i++) {
        printf("%d", recipes[INPUT + i]);
    }
    printf("\n");

    printf("Part 2: ");
    int pattern[] = INPUT_ARR;
    int pattern_len = sizeof(pattern) / sizeof(*pattern);
    for (int i = 0; i < num_recipes - pattern_len; i++) {
        int match = 1;
        for (int j = 0; j < pattern_len; j++) {
            if (recipes[i + j] != pattern[j]) {
                match = 0;
                break;
            }
        }
        if (match == 1) {
            printf("%d", i);
            break;
        }
    }
    printf("\n");
}
