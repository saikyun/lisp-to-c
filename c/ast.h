typedef enum AstType {
  NIL,
  CALL,
  IF,
  SYMBOL,
  QUOTE
} AstType;

char *type_name(AstType type) {
  switch (type) {
    case NIL:    return "nil";
    case CALL:   return "call";
    case IF:     return "if";
    case SYMBOL: return "symbol";
    case QUOTE:  return "quote";
  }
}

typedef char* AstSymbol;

typedef struct Ast Ast;

typedef struct AstCall {
  char *func;
  Ast *args;
  int nof_args;
} AstCall;

typedef struct AstIf {
  Ast *pred;
  Ast *true_branch;
  Ast *false_branch;
} AstIf;

typedef struct Ast {
  AstType type;
  union {
    AstCall call;
    AstIf ast_if;
    AstSymbol symbol;
    Ast *quote;
  };
} Ast;

Ast nil = {.type = NIL};