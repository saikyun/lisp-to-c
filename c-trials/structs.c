#include<stdio.h>
#include<stdlib.h>
#include<time.h>

typedef enum Kind {
  NIL,
  PLAYER,
  ENEMY
} Kind;

Kind kind(void *o) {
  return *((Kind *)o);
}

typedef struct Player {
  Kind kind;
  int hp;
  int attack;
} Player;

typedef struct Enemy {
  Kind kind;
  int attack;
  int hp;
} Enemy;

int Player_get_attack(Player *p) {
  return p->attack;
}

int Enemy_get_attack(Enemy *p) {
  return p->attack;
}

int Enemy_set_hp(Enemy *e, int new_hp) {
  e->hp = e->hp - new_hp;
}

int Player_set_hp(Player *e, int new_hp) {
  e->hp = e->hp - new_hp;
}

int get_attack(void *o1) {
  switch (kind(o1)) {
    case PLAYER: return Player_get_attack((Player *)o1); break;
    case ENEMY: return Enemy_get_attack((Enemy *)o1); break;
  }
}

void set_hp(void *o, int hp) {
  switch (kind(o)) {
    case ENEMY: Enemy_set_hp((Enemy *)o, hp); break;
    case PLAYER: Player_set_hp((Player *)o, hp); break;
  }
}

void attack(void *o1, void *o2) {
  set_hp(o2, get_attack(o1));
}

int lule(int r) {
    Player p = {.attack = r, .kind = PLAYER};
    Enemy e = {.attack = 10, .hp = 50, .kind = ENEMY};
    attack(&p, &e);
    return e.hp;
}

int main() {
    srand(time(NULL));
    int r = rand() % 10;
    printf("r is: %d, lule is: %d\n", r, lule(r));
    return 0;
}