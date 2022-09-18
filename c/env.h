#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"

typedef enum ValueType {
  BOOL,
  AST,
  STRING
} ValueType;

typedef struct Value {
  ValueType type;
  char *name;
  union {
    bool boolean;
    Ast macro;
    char *string;
  };
} Value;

typedef struct Env {
  Value *values;
  int count;
  int size;
} Env;

void push_value(Env *env, Value v) {
  assert(env->count < env->size);

  env->values[env->count] = v;
  env->count += 1;
}


Value *push(Env *env, char *name) {
  assert(env->count < env->size);

  env->count += 1;

  env->values[env->count - 1].name = name;

  return &env->values[env->count - 1];
}

void set_bool(Env *env, char *name, bool value) {
  Value *v = push(env, name);
  v->boolean = value;
  v->type = BOOL;
}

void set_macro(Env *env, char *name, Ast m) {
  Value *v = push(env, name);
  v->macro = m;
  v->type = AST;
}

void set_string(Env *env, char *name, char *m) {
  Value *v = push(env, name);
  v->string = m;
  v->type = STRING;
}

#define UNREACHABLE false

Value *get(Env *env, char *name) {
  for (int i = 0; i < env->count; i++) {
    if (strcmp(name, env->values[i].name) == 0) {
      return &env->values[i];
    }
  }
  assert(UNREACHABLE);
}

Env *new_env(int size) {
  Env *env = (Env *)malloc(sizeof(Env));
  env->size = size;
  env->values = (Value *)malloc(sizeof(Value) * env->size);
  env->count = 0;
  return env;
}