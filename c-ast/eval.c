#include "env.h"

#include <stdio.h>
#include <stdlib.h>

bool truthy(Value *v) {
  switch (v->type) {
    case BOOL:
      return v->boolean;
  }
}

Value eval(Env *env, Ast *ast);

Value eval_list(Env *env, AstList list) {
  assert(list.length > 0);
  Ast head = list.elems[0];
  assert(head.type == AST_SYMBOL);
  Value *f = get(env, head.symbol);
  assert(f);
  Value args[] = { eval(env, &list.elems[1]), eval(env, &list.elems[2]) };
  return f->function(args, 2);
}

Value eval(Env *env, Ast *ast) {
  printf("%s\n", type_name(ast->type));
  switch (ast->type) {
    case AST_NUMBER:
      return (Value){.type = NUMBER, .number = ast->number};
    case LIST:
      return eval_list(env, ast->list);
    default:
      printf("can't eval %s", type_name(ast->type));
      exit(1);
  }
}

Value plus(Value vs[], size_t length) {
  float x = vs[0].number;
  float y = vs[1].number;
  return (Value){.type = NUMBER, .number = x + y};
}

int main() {
  Env *env = new_env(50);
  set_bool(env, "debug", true);

  set_func(env, "+", plus);

  Ast ast = {.type = AST_NUMBER, .number = 5.0f};
  Value v = eval(env, &ast);
  print_value(&v);

  Ast ast2 = {.type = LIST,
              .list = (AstList){
                .elems = (Ast[]){
                  {.type = AST_SYMBOL, .symbol = "+" },
                  {.type = AST_NUMBER, .number = 5.0f },
                  {.type = AST_NUMBER, .number = 9.0f } },
                .length = 3 } };
  Value v2 = eval(env, &ast2);
  print_value(&v2);
}