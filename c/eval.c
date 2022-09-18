#include "env.h"

#include <stdio.h>
#include <stdlib.h>

bool truthy(Value *v) {
  switch (v->type) {
    case BOOL:
      return v->boolean;
  }
}

Value call(Env *env, AstCall *call) {
  printf("calling : %s\n", call->func);
}

Value eval(Env *env, Ast *ast) {
  printf("%s\n", type_name(ast->type));
  switch (ast->type) {
    case IF:
      AstIf a = ast->ast_if;
      Value res = eval(env, a.pred);
      if (truthy(&res)) {
        return eval(env, a.true_branch);
      }
      break;
    case CALL:
      return call(env, &ast->call);
    case SYMBOL:
      return *get(env, ast->symbol);
    case QUOTE:
      return (Value){.type = AST, .macro = *ast->quote};
    default:
      printf("unhandled node type %s\n", type_name(ast->type));
      break;
  }
}

Ast eval_macro(Env *env, Ast *macro) {
  Value res = eval(env, macro);
  printf("type of res: %s\n", type_name(res.macro.type));
  eval(env, &res.macro);
}

int main() {
  Env *env = new_env(50);
  set_bool(env, "debug", true);
  /*
  (defmacro log
    [v]
    ~(if debug
       (print ,v)))

  =>
  if (get(env, "debug").bool)
    return (Ast){.call = "print"
                 .args = [v]}
  =>
  eval(ast);
  print(args);
  */

  int xs[] = { 1, 2 };

/*
  Ast args[] = (Ast[]){ (Ast){ .type = QUOTE .quote = { .type = SYMBOL, .symbol = "print" } },
                 (Ast){ .type = SYMBOL, .symbol = "v" } };
*/

  set_macro(
    env,
    "log",
    (Ast){
      .type = IF,
      .ast_if = {
        .pred = &(Ast){
          .type = SYMBOL,
          .symbol = "debug"
        },
        .true_branch = &(Ast){
          .type = QUOTE,
          .quote = &(Ast){
            .type = CALL,
            .call = {
              .func = "tuple",
              .nof_args = 2,
              .args = (Ast[]){ (Ast){ .type = QUOTE, .quote = &(Ast){ .type = SYMBOL, .symbol = "print" } },
                 (Ast){ .type = SYMBOL, .symbol = "v" } }
            }
          }
        }
      }
    }
  );

  //char buffer[50];
  //char *res = fgets(buffer, 50, stdin);
  
  set_string(env, "name", "jona" //res
);

  Ast m = get(env, "log")->macro;
  eval_macro(env, &m);
}