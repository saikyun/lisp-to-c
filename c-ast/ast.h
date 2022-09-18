typedef enum AstType {
  AST_STRING,
  AST_NUMBER,
  AST_SYMBOL,
  LIST
} AstType;

char *type_name(AstType type) {
  switch (type) {
  case AST_NUMBER: return "number";
  case AST_STRING: return "string";
  case AST_SYMBOL: return "symbol";
  case LIST:   return "list";
  }
}

typedef float AstNumber;
typedef char* AstSymbol;
typedef char* AstString;

typedef struct Ast Ast;

typedef struct AstList {
  Ast *elems;
  int length;
} AstList;

typedef struct Ast {
  AstType type;
  union {
    AstNumber number;
    AstSymbol symbol;
    AstString string;
    AstList list;
  };
} Ast;