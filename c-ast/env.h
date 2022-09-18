#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ast.h"

typedef enum ValueType {
  STRING,
  NUMBER,
  BOOL,
  AST,
  FUNCTION
} ValueType;

typedef struct Value {
  ValueType type;
  union {
    bool boolean;
    Ast ast;
    char *string;
    float number;
    struct Value (*function)(struct Value *, size_t);
  };
} Value;

void print_value(Value *v) {
  switch (v->type) {
    case STRING: printf("%s\n", v->string); break;
    case NUMBER: printf("%f\n", v->number); break;
    default: 
      printf("can't print %d", v->type);
      exit(1);
  }
}

typedef struct ValueInEnv {
  char *name;
  Value value;
} ValueInEnv;

typedef struct Env {
  ValueInEnv *values;
  int count;
  int size;
} Env;



ValueInEnv *push(Env *env, char *name) {
  assert(env->count < env->size);

  env->count += 1;

  env->values[env->count - 1].name = name;

  return &env->values[env->count - 1];
}

void set_bool(Env *env, char *name, bool value) {
  ValueInEnv *v = push(env, name);
  v->value.boolean = value;
  v->value.type = BOOL;
}

void set_ast(Env *env, char *name, Ast m) {
  ValueInEnv *v = push(env, name);
  v->value.ast = m;
  v->value.type = AST;
}

void set_string(Env *env, char *name, char *m) {
  ValueInEnv *v = push(env, name);
  v->value.string = m;
  v->value.type = STRING;
}

void set_func(Env *env, char *name, Value (*func)(Value *, size_t)) {
  ValueInEnv *v = push(env, name);
  v->value.function = func;
  v->value.type = FUNCTION;
}

#define UNREACHABLE false

Value *get(Env *env, char *name) {
  for (int i = 0; i < env->count; i++) {
    if (strcmp(name, env->values[i].name) == 0) {
      return &env->values[i].value;
    }
  }
  printf("could not find %s in env", name);
  exit(1);
}

Env *new_env(int size) {
  Env *env = (Env *)malloc(sizeof(Env));
  env->size = size;
  env->values = (ValueInEnv *)malloc(sizeof(ValueInEnv) * env->size);
  env->count = 0;
  return env;
}