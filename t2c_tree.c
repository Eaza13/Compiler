#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "t2c.h"
#include "t2c_tree.h"

char c_types1[3][6] = {"int", "float", "char*"};
char c_types2[3][5] = {"int", "real", "str"};

tEXP* create_exp ( ) {
  tEXP* tmp;
  tmp = (struct t_exp *)malloc(sizeof(struct t_exp));
  if( tmp ) {
    tmp->exp_id = eMIN;
    tmp->name[0] = '\0';
    tmp->ival = 0;
    tmp->rval = 0.0;
    tmp->qstr[0] = '\0';
    tmp->exp1 = NULL;
    tmp->next = NULL;
  }
  return tmp;
}

tSTM* create_stm ( ) {
  tSTM* tmp;
  tmp = (struct t_stm *)malloc(sizeof(struct t_stm));
  if( tmp ) {
    tmp->stm_id = sMIN;
    tmp->exp1 = NULL;
    tmp->exp2 = NULL;
    tmp->stm1 = NULL;
    tmp->stm2 = NULL;
    tmp->next = NULL;
  }
  return tmp;
}

symNODE* create_symnode ( char* s, int t ) {
  symNODE* tmp;
  tmp = (struct sym_node *)malloc(sizeof(struct sym_node));
  if( tmp ) {
    strcpy( tmp->name, s );
    tmp->type = t;
    tmp->next = symtab;
  }
  return tmp;
}

void free_exp ( tEXP* p ) {
  if( p ) {
    if( p->exp1 ) free_exp( p->exp1 );
    if( p->next ) free_exp( p->next );
    free( p );
  }
}

void free_stm ( tSTM* p ) {
  if( p ) {
    if( p->exp1 ) free_exp( p->exp1 );
    if( p->exp2 ) free_exp( p->exp2 );
    if( p->stm1 ) free_stm( p->stm1 );
    if( p->stm2 ) free_stm( p->stm2 );
    if( p->next ) free_stm( p->next );
    free( p );
  }
}

void free_symnode ( symNODE* p ) {
  if( p ) {
    if( p->next ) free_symnode( p->next );
    free( p );
  }
}

void print_exp ( tEXP* p ) {
  tEXP* te;
  if( p ) {
    switch( p->exp_id ) {
    case eTYPE: printf("eTYPE: %d\n", p->ival);
		break;
    case eFORM: printf("eFORM: ");
		print_exp( p->exp1 );
		printf(" %s", p->name);
		if (p->next) {
		    printf(", ");
		    print_exp( p->next );
		}
		printf("(end of eFORM)\n");
		break;
    case eEXPR: printf("eEXPR: ");
		print_exp( p->exp1 );
		if (p->next) {
		    print_exp( p->next );
		}
		printf("(end of eEXPR)\n");
		break;
    case eADD:  printf("eADD: ");
		print_exp( p->exp1 );
		if (p->next) {
		    print_exp( p->next );
		}
		printf("(end of eADD)\n");
		break;
    case eMINUS: printf("eMINUS: ");
		print_exp( p->exp1 );
		if (p->next) {
		    print_exp( p->next );
		}
		printf("(end of eMINUS)\n");
		break;
    case eMEXP: printf("eMEXP: ");
		print_exp( p->exp1 );
		if (p->next) {
		    print_exp( p->next );
		}
		printf("(end of eMEXP)\n");
		break;
    case eTIMES: printf("eTIMES: ");
		print_exp( p->exp1 );
		if (p->next) {
		    print_exp( p->next );
		}
		printf("(end of eTIMES)\n");
		break;
    case eDIVIDE: printf("eDIVIDE: ");
		print_exp( p->exp1 );
		if (p->next) {
		    print_exp( p->next );
		}
		printf("(end of eDIVIDE)\n");
		break;
    case eINUM: printf("eINUM: %d\n", p->ival);
		break;
    case eRNUM: printf("eRNUM: %f\n", p->rval);
		break;
    case eID:   printf("eID: %s\n", p->name);
		break;
    case ePAREN: printf("(\n");
		print_exp( p->exp1 );
		printf(")\n");
		break;
    case eFUNC: printf("eFUNC: %s(", p->name);
		print_exp( p->exp1 );
		printf(")(end of eFUNC)\n");
		break;
    case eEQ:   printf("eEQ: ");
		print_exp( p->exp1 );
		printf("(end of eEQ)\n");
		break;
    case eNE:   printf("eNE: ");
		print_exp( p->exp1 );
		printf("(end of eNE)\n");
		break;
    case eAPARM: printf("eAPARM: ");
                print_exp( p->exp1 );
		if (p->next) print_exp( p->next );
		printf("(end of eAPRAM)\n");
		break;
    case eASSIGN1: printf("eASSIGN1: %s = ", p->name);
		print_exp( p->exp1 );
		printf("(end of eASSIGN1)\n");
		break;
    case eASSIGN2: printf("eASSIGN2: %s = %s\n", p->name, p->qstr);
		break;
    case eWSTM: printf("eWSTM: WRITE( ");
		print_exp( p->exp1 );
		printf(", %s )\n", p->qstr);
		break;
    case eDSTM: printf("eDSTM: READ( %s, %s )\n", p->name, p->qstr);
		break;
    default: fprintf(stderr, "******* An error in expressions!\n");
	     break;
    }
  }
}

void print_stm ( tSTM* p ) {
  tEXP *te;
  tSTM *ts;

  if( p ) {
    switch( p->stm_id ) {
    case sMAIN: printf("sMAIN: ( ");
		print_exp( p->exp2 );
		printf(" ) ");
		print_stm( p->stm1 );
		break;
    case sMDCL: printf("sMDCL: %s ( ", p->exp1->name );
		print_exp( p->exp2 );
		printf(" ) ");
		print_stm( p->stm1 );
		break;
    case sBLOCK: printf("sBLOCK: {\n");
		print_stm( p->stm1 );
		printf(" }\n");
		break;
    case sVDCL1: printf("sVDCL1: ");
		print_exp( p->exp1 );
		printf("%s\n", p->exp1->name); 
		break;
    case sVDCL2: printf("sVDCL2: ");
		print_exp( p->exp1 );
		print_stm( p->stm1 );
		break;
    case sASTM: printf("sASTM: ");
		print_exp( p->exp1 );
		break;
    case sRSTM: printf("sRSTM: return ");
		print_exp( p->exp1 );
		break;
    case sISTM: printf("sISTM: if ( ");
		print_exp( p->exp1 );
		print_stm( p->stm1 );
		if (p->stm2) {
		    printf("    else\n");
		    print_stm( p->stm2 );
		}
		break;
    case sWSTM: printf("sWSTM: ");
		print_exp( p->exp1 );
		break;
    case sDSTM: printf("sDSTM: ");
		print_exp( p->exp1 );
		break;
    default: fprintf(stderr, "******* An error in statements!\n");
	     break;
    }
    if (p->next) print_stm( p->next );
  }
}

void gen_exp ( tEXP* p ) {
  tEXP* te;
  if( p ) {
    switch( p->exp_id ) {
    case eTYPE: fprintf( yyout, "%s", c_types1[ p->ival - 1] );
		break;
    case eFORM: gen_exp( p->exp1 );
		fprintf( yyout, " %s", p->name );
		if (p->next) {
		    fprintf( yyout, ", " );
		    gen_exp( p->next );
		}
		break;
    case eEXPR: gen_exp( p->exp1 );
		gen_exp( p->next );
		break;
    case eADD:  fprintf( yyout, " + " );
		gen_exp( p->exp1 );
		gen_exp( p->next );
		break;
    case eMINUS: fprintf( yyout, " - " );
		gen_exp( p->exp1 );
		print_exp( p->next );
		break;
    case eMEXP: fprintf(yyout, " * ");
        gen_exp(p->exp1);
        gen_exp(p->next); // Write your own gen_exp here.
		break;
    case eTIMES: fprintf(yyout, " * ");
        gen_exp(p->exp1);
        gen_exp(p->next); // Write your own gen_exp here.
		break;
    case eDIVIDE: fprintf(yyout, " / ");
        gen_exp(p->exp1);
        gen_exp(p->next);// Write your own gen_exp here.
		break;
    case eINUM: fprintf( yyout, "%d", p->ival );
		break;
    case eRNUM: fprintf( yyout, "%f", p->rval );
		break;
    case eID:   fprintf( yyout, "%s", p->name );
		break;
    case ePAREN:  fprintf(yyout, "(");
        gen_exp(p->exp1);
        fprintf(yyout, ")");// Write your own gen_exp here.
		break;
    case eFUNC: fprintf(yyout, "%s(", p->name);
        gen_exp(p->exp1);
        fprintf(yyout, ")");// Write your own gen_exp here.
		break;
    case eEQ:  fprintf(yyout, " == ");
        gen_exp(p->exp1);
        gen_exp(p->next);// Write your own gen_exp here.
		break;
    case eNE:  fprintf(yyout, " != ");
        gen_exp(p->exp1);
        gen_exp(p->next);// Write your own gen_exp here.
		break;
    case eGT:  fprintf(yyout, " > ");
        gen_exp(p->exp1);
        gen_exp(p->next);// Write your own gen_exp here.
		break;
    case eLT:  fprintf(yyout, " < ");
        gen_exp(p->exp1);
        gen_exp(p->next);// Write your own gen_exp here.
		break;
    case eGE:  fprintf(yyout, " >= ");
        gen_exp(p->exp1);
        gen_exp(p->next);// Write your own gen_exp here.
		break;
    case eLE:  fprintf(yyout, " <= ");
        gen_exp(p->exp1);
        gen_exp(p->next);// Write your own gen_exp here.
		break;
    case eAPARM: gen_exp(p->exp1);
        if (p->next) {
          fprintf(yyout, ", ");
          gen_exp(p->next);
        }// Write your own gen_exp here.
		break;
    case eASSIGN1: fprintf( yyout, "%s = ", p->name );
		gen_exp( p->exp1 );
		fprintf( yyout, ";\n" );
		break;
    case eASSIGN2: fprintf( yyout, "%s = %s;\n", p->name, p->qstr );
		break;
    case eWSTM: fprintf(yyout, "printf(\"%s\", ", p->qstr);
        gen_exp(p->exp1);
        fprintf(yyout, ");\n");// Write your own gen_exp here.
		break;
    case eDSTM: if (p->ival == tINT) {
        fprintf(yyout, "scanf(\"%%d\", &%s);\n", p->name);
        } else if (p->ival == tREAL) {
          fprintf(yyout, "scanf(\"%%f\", &%s);\n", p->name);
        } else {
          fprintf(yyout, "scanf(\"%%s\", %s);\n", p->name);
        }// Write your own gen_exp here.
		break;
    default: fprintf(stderr, "******* An error in expressions!\n");
	     break;
    }
  }
}

void gen_code ( tSTM* p ) {
  tEXP *te;
  tSTM *ts;
  int t;

  if( p ) {
    switch( p->stm_id ) {
    case sMAIN: gen_exp( p->exp1->exp1 );
		fprintf( yyout, " main ( " );
		gen_exp( p->exp2 );
		fprintf( yyout, ")\n" );
		gen_code( p->stm1 );
		break;
    case sMDCL: gen_exp( p->exp1->exp1 );
		fprintf( yyout, " " );
		gen_exp( p->exp1 );
		fprintf( yyout, " ( " );
		gen_exp( p->exp2 );
		fprintf( yyout, ")\n" );
		gen_code( p->stm1 );
		break;
    case sBLOCK: fprintf( yyout, "{ ");
		gen_code( p->stm1 );
		fprintf( yyout, "}\n");
		break;
    case sVDCL1: gen_exp( p->exp1 );
		fprintf( yyout, " %s;\n", p->exp1->name ); 
		break;
    case sVDCL2: gen_exp( p->exp1 );
		fprintf( yyout, " " ); 
		gen_code( p->stm1 );
		break;
    case sASTM: gen_exp( p->exp1 );
		break;
    case sRSTM: fprintf(yyout, "return ");
        gen_exp(p->exp1);
        fprintf(yyout, ";\n");// Write your own gen_code here.
		break;
    case sISTM: fprintf(yyout, "if (");
        gen_exp(p->exp1);
        fprintf(yyout, ") ");
        gen_code(p->stm1);
        if (p->stm2) {
          fprintf(yyout, " else ");
          gen_code(p->stm2);
        }// Write your own gen_code here.
		break;
    case sWSTM: gen_exp(p->exp1);// Write your own gen_code here.
		break;
    case sDSTM: gen_exp(p->exp1);// Write your own gen_code here.
		break;
    default:    fprintf(stderr, "******* An error in statements!\n");
		break;
    }
    if (p->next) gen_code( p->next );
  }
}

int lookup( symNODE* p, char* s ) {
  int tmp = 0;
  if (p) {
    if (strcmp( p->name, s ) == 0) {
      tmp = p->type;
    } else {
      tmp = lookup( p->next, s );
    }
  }
  return tmp;
}

int ana_exptype( tEXP *p ) {
  // Will get bonus if you implement an ana_exptype function.
  int tmp = tINT;
  //begin implementation
  if (p) {
        ana_exptype(p->exp1);
        if (p->next) {
            ana_exptype(p->next);
        }

        switch (p->exp_id) {
            case eINUM:
                p->ival = tINT;
                tmp = tINT;
                break;
            case eRNUM:
                p->ival = tREAL;
                tmp = tREAL;
                break;
            case eID:
                p->ival = lookup(symtab, p->name);
                tmp = p->ival;
                break;
            case eADD:
            case eMINUS:
            case eTIMES:
            case eDIVIDE:
                if (p->exp1->ival == tREAL || (p->next && p->next->ival == tREAL)) {
                    p->ival = tREAL;
                    tmp = tREAL;
                } else {
                    p->ival = tINT;
                    tmp = tINT;
                }
                break;
            default:
                p->ival = tINT;
                tmp = tINT;
                break;
        }
    } // end implementation
  return tmp;
}

void init_all () {
    int i, j;
    // initialize gen_rw
    for (i=0; i<2; i++)
	for (j=0; j<3; j++) gen_rw[i][j] = 0;
    printf("mytiny2c: init_all() done!\n");
}

void gen_rwcode () {
    int i, j;

// Combine init_code
  fprintf( yyout, "#include <stdio.h>\n");
  fprintf( yyout, "#include <stdlib.h>\n");
  fprintf( yyout, "#include <string.h>\n");

// Real gen_rwcode
    if (gen_rw[0][0] == 1) {
	fprintf( yyout, "void tiny_readint ( int *x, char *s ) {\n");
        fprintf( yyout, "  printf(\"%%s \", s);\n");
        fprintf( yyout, "  scanf(\"%%d\", x);\n");
        fprintf( yyout, "}\n");
    }
    if (gen_rw[0][1] == 1) {
	fprintf( yyout, "void tiny_readreal ( float *x, char *s ) {\n");
        fprintf( yyout, "  printf(\"%%s \", s);\n");
        fprintf( yyout, "  scanf(\"%%f\", x);\n");
        fprintf( yyout, "}\n");
    }
    if (gen_rw[0][2] == 1) {
	fprintf( yyout, "void tiny_readstr ( char *x, char *s ) {\n");
        fprintf( yyout, "  printf(\"%%s \", s);\n");
        fprintf( yyout, "  scanf(\"%%s\", x);\n");
        fprintf( yyout, "}\n");
    }
    if (gen_rw[1][0] == 1) {
	fprintf( yyout, "void tiny_writeint ( int x, char *s ) {\n");
        fprintf( yyout, "  printf(\"%%s \", s);\n");
        fprintf( yyout, "  printf(\"%%d\\n\", x);\n");
        fprintf( yyout, "}\n");
    }
    if (gen_rw[1][1] == 1) {
	fprintf( yyout, "void tiny_writereal ( float x, char *s ) {\n");
        fprintf( yyout, "  printf(\"%%s \", s);\n");
        fprintf( yyout, "  printf(\"%%f\\n\", x);\n");
        fprintf( yyout, "}\n");
    }
    if (gen_rw[1][2] == 1) {
	fprintf( yyout, "void tiny_writestr ( char* x, char *s ) {\n");
        fprintf( yyout, "  printf(\"%%s \", s);\n");
        fprintf( yyout, "  printf(\"%%s\\n\", x);\n");
        fprintf( yyout, "}\n");
    }
}

