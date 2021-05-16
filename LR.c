#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

/* LR(0) PARSER GENERATOR */
/* Input format derived from the following syntax:
   Grammar = Rule+.
   Rule = "*" ID "." |
          ID "=" ID* ("|" ID*)* "." |
          ID ":" ID+ ".".
 */

/* THE SCANNER */
typedef char Lexical;
char *LastW;

#define MAX_CHAR 0x4000
static char ChArr[MAX_CHAR], *ChP = ChArr;

static int LINE = 1, ERRORS = 0;
#define MAX_ERRORS 25

static int GET(void) {
   int Ch = getchar();
   if (Ch == '\n') LINE++;
   return Ch;
}

static void UNGET(int Ch) {
   if (Ch == '\n') --LINE;
   ungetc(Ch, stdin);
}

static void ERROR(char *Format, ...) {
   va_list AP;
   fprintf(stderr, "[%d] ", LINE);
   va_start(AP, Format); vfprintf(stderr, Format, AP); va_end(AP);
   fputc('\n', stderr);
   if (++ERRORS == MAX_ERRORS)
      fprintf(stderr, "Reached the %d error limit.\n", MAX_ERRORS), exit(1);
}

Lexical LEX(void) {
   int Ch;
   do Ch = GET(); while (isspace(Ch));
   switch (Ch) {
      case EOF: return 0;
      case '|': case '*': case ':': case '=': case '.': return Ch;
   }
   if (isalpha(Ch) || Ch == '_') {
      for (LastW = ChP; isalnum(Ch) || Ch == '_'; ChP++) {
         if (ChP - ChArr == MAX_CHAR)
            printf("Out of character space.\n"), exit(1);
         *ChP = Ch, Ch = GET();
      }
      if (Ch != EOF) UNGET(Ch);
      if (ChP - ChArr == MAX_CHAR) printf("Out of character space.\n"), exit(1);
      *ChP++ = '\0';
      return 'x';
   } else if (Ch == '"') {
      Ch = GET();
      for (LastW = ChP; Ch != '"' && Ch != EOF; ChP++) {
         if (ChP - ChArr == MAX_CHAR)
            printf("Out of character space.\n"), exit(1);
         *ChP = Ch, Ch = GET();
      }
      if (Ch == EOF) printf("Missing closing \".\n"), exit(1);
      if (ChP - ChArr == MAX_CHAR) printf("Out of character space.\n"), exit(1);
      *ChP++ = '\0';
      return 'x';
   } else {
      ERROR("extra character %c", Ch); return 0;
   }
}

/* DATA STRUCTURES */
typedef unsigned char byte;

/* A literal symbol, L, has the following format:
      Literal = 1
      Rules == 0

   A non-terminal symbol, S, has the following format:
      Literal = 0
      RList[i] = { a1, ..., an, NULL } if S -> a1 ... an

   RList is a dynamic array, growing 8 items at a time.

   Symbols are hashed into HashTab, linked by the Next pointer.
   They are listed in the order they are encountered in FirstB..LastB,
   linked by the Tail pointer.
 */  

typedef struct Symbol *Symbol;
typedef Symbol *Rule;
struct Symbol {
   char *Name; byte Literal; unsigned Index;
   unsigned Rules; Rule *RList;
   Symbol Next, Tail;
};

void *Allocate(unsigned Bytes) {
   void *X = malloc(Bytes);
   if (X == 0) printf("Out of memory.\n"), exit(1);
   return X;
}

void *Reallocate(void *X, unsigned Bytes) {
   X = realloc(X, Bytes);
   if (X == 0) printf("Out of memory.\n"), exit(1);
   return X;
}

#define HASH_MAX 0x100
static Symbol HashTab[HASH_MAX], FirstB = 0, LastB;

char *CopyS(char *S) {
   char *NewS = (char *)Allocate(strlen(S) + 1);
   strcpy(NewS, S); return NewS;
}

byte Hash(char *S) {
   int H; char *T;
   for (H = 0, T = S; *T != '\0'; T++) H = (H << 1) ^ *T;
   return H&0xff;
}

Symbol LookUp(char *S) {
   static int LABEL = 0;
   Symbol Sym; byte H;
   for (H = Hash(S), Sym = HashTab[H]; Sym != 0; Sym = Sym->Next)
      if (strcmp(Sym->Name, S) == 0) return Sym;
   Sym = (Symbol)Allocate(sizeof *Sym);
   Sym->Name = CopyS(S), Sym->Literal = 1, Sym->Index = LABEL++;
   Sym->Rules = 0, Sym->RList = 0;
   Sym->Next = HashTab[H], HashTab[H] = Sym;
   Sym->Tail = 0;
   if (FirstB == 0) FirstB = Sym; else LastB->Tail = Sym;
   return LastB = Sym;
}

#define MAX_SYM 0x100
Symbol SymBuf[MAX_SYM], *SymP;

void InsertR(Symbol S) {
   Rule R; int I, J, Diff; Symbol *A, *B;
   for (I = 0; I < S->Rules; I++) {
      for (Diff = 0, A = SymBuf, B = S->RList[I]; *A != 0 && *B != 0; A++, B++) {
         Diff = (*A)->Index - (*B)->Index;
         if (Diff != 0) break;
      }
      if (Diff == 0 && *B == 0) {
         if (*A == 0) return; else break;
      } else if (Diff > 0) break;
   }
   if ((S->Rules&7) == 0)
      S->RList = Reallocate(S->RList, sizeof *S->RList*(S->Rules + 8));
   for (J = S->Rules++; J > I; J--) S->RList[J] = S->RList[J - 1];
   S->RList[I] = R = Allocate(sizeof *R * (SymP - SymBuf));
   for (I = 0; I < (SymP - SymBuf); I++) R[I] = SymBuf[I];
}

Symbol Grammar(void) {
   Symbol Start = 0, LHS, Sym; Lexical L = LEX(); int SawStart = 0;
START:
   switch (L) {
      case 0: return Start;
      case 'x': goto EQUAL;
      case '.': L = LEX(); goto START;
      case ':': case '=':
	 ERROR("Missing left-hand side of rule, or '.' from previous rule.");
      goto FLUSH;
      case '*':
         L = LEX();
         if (L != 'x') ERROR("Missing symbol after '*'.");
         else {
            Start = LookUp(LastW);
            if (SawStart++ > 0) ERROR("Start symbol redefined.");
            L = LEX();
         }
      goto FLUSH;
      default: case '|': ERROR("Corrupt rule."); goto FLUSH;
   }
FLUSH:
   for (; L != '.' && L != 0; L = LEX()) ;
goto END;
EQUAL:
   LHS = LookUp(LastW); if (Start == 0) Start = LHS;
   L = LEX(); LHS->Literal = 0;
   if (L == ':') {
      for (L = LEX(); L == 'x'; L = LEX()) {
         Symbol X = LookUp(LastW);
         SymP = SymBuf, *SymP++ = X, *SymP++ = 0, InsertR(LHS);
      }
   } else if (L == '=') {
      do {
         L = LEX();
         for (SymP = SymBuf; L == 'x' && SymP < SymBuf + MAX_SYM; L = LEX())
            *SymP++ = LookUp(LastW);
         if (SymP >= SymBuf + MAX_SYM) printf("Large rule.\n"), exit(1);
         *SymP++ = 0; InsertR(LHS);
      } while (L == '|');
   } else {
      ERROR("Missing '=' or ':'."); goto FLUSH;
   }
END:
   if (L == 0) { ERROR("Missing '.'"); return Start; }
   if (L == '.') L = LEX();
goto START;
}

/* Item: LHS -> RHS[0] ... RHS[n-1] $ RHS[n] ... RHS[p-1] NULL
         Pos == &RHS[n], 0 <= n <= p
   Items: { List[0], List[1], ..., List[Size-1] }
          Pre is the accessing symbol in the itemset construction,
          or NULL for the initial item.
 */
typedef struct Item { Symbol LHS, *RHS, *Pos; } *Item;
typedef struct Items { Symbol Pre; int Size; Item *List; } *Items;

Items STab; int Ss;

Item CopyI(Item A) {
   Item B = Allocate(sizeof *B);
   *B = *A;
   return B;
}

int CompI(Item A, Item B) {
   int Diff; Rule AP, BP;
   Diff =
      A->LHS == 0? (B->LHS == 0? 0: -1):
      B->LHS == 0? +1: A->LHS->Index - B->LHS->Index;
   if (Diff != 0) return Diff;
   Diff = (A->Pos - A->RHS) - (B->Pos - B->RHS); if (Diff != 0) return Diff;
   for (AP = A->RHS, BP = B->RHS; *AP != 0 && *BP != 0; AP++, BP++)
      if ((Diff = (*AP)->Index - (*BP)->Index) != 0) break;
   return *AP == 0? (*BP == 0? 0: -1): *BP == 0? +1: Diff;
}

typedef struct Reduce { int Q; Symbol LHS; Rule RHS; } *Reduce;
typedef struct Shift { int Q; Symbol X; int R; } *Shift;

Reduce RList = 0; unsigned Rs = 0;
Shift SList = 0; unsigned Shs = 0;

int CompRd(Reduce A, int Q, Symbol LHS, Rule RHS) {
   int Diff; Rule AP, BP;
   Diff = (A->LHS == 0) - (LHS == 0); if (Diff != 0) return Diff;
   if (A->LHS != 0 && LHS != 0) {
      Diff = (A->LHS->Index - LHS->Index); if (Diff != 0) return Diff;
   }
   for (AP = A->RHS, BP = RHS; *AP != 0 && *BP != 0; AP++, BP++)
      if ((Diff = (*AP)->Index - (*BP)->Index) != 0) return Diff;
   Diff = (*BP == 0) - (*AP == 0); if (Diff != 0) return Diff;
   return A->Q - Q;
}

int CompSh(Shift A, int Q, Symbol X, int R) {
   int Diff;
   Diff = (A->X == 0) - (X == 0); if (Diff != 0) return Diff;
   if (A->X != 0 && X != 0) {
      Diff = (A->X->Index - X->Index); if (Diff != 0) return Diff;
   }
   Diff = (A->R - R); if (Diff != 0) return Diff;
   return A->Q - Q;
}

void ShowShifts(void) {
   Shift S;
   for (S = SList; S < SList + Shs; ) {
      Shift S1;
      if (S->X->Literal) { S++; continue; }
      printf("%sF -> [%d", S->X->Name, S->Q);
      for (S1 = S++; S < SList + Shs; S++) {
         if (S->X != S1->X || S->R != S1->R) break;
         printf(",%d", S->Q);
      }
      printf("] <%d| Q\n", S1->R);
   }
   for (S = SList; S < SList + Shs; ) {
      Shift S1;
      if (!S->X->Literal) { S++; continue; }
      printf("Q -> [%d", S->Q);
      for (S1 = S++; S < SList + Shs; S++) {
         if (S->X != S1->X || S->R != S1->R) break;
         printf(",%d", S->Q);
      }
      printf("] \"%s\" <%d| Q\n", S1->X->Name, S1->R);
   }
}

void ShowReduces(void) {
   Reduce Rd;
   for (Rd = RList; Rd < RList + Rs; ) {
      Reduce Rd1; Rule R;
      printf("Q -> [%d", Rd->Q);
      for (Rd1 = Rd++; Rd < RList + Rs; Rd++) {
         if (Rd->LHS != Rd1->LHS || Rd->RHS != Rd1->RHS) break;
         printf(",%d", Rd->Q);
      }
      printf("] ");
      if (Rd1->LHS == 0) printf("{accept}\n");
      else {
         printf("{%s =>", Rd1->LHS->Name);
         if (Rd1->RHS != 0)
            for (R = Rd1->RHS; *R != 0; R++) printf(" %s", (*R)->Name);
         printf("}\n");
      }
   }
}

int AddState(int Size, Item *List) {
   int I, S; Items IS;
   for (S = 0; S < Ss; S++) {
      IS = &STab[S];
      if (IS->Size != Size) continue;
      for (I = 0; I < IS->Size; I++)
         if (CompI(IS->List[I], List[I]) != 0) break;
      if (I >= IS->Size) { free(List); return S; }
   }
   if (Ss%0x10 == 0) STab = Reallocate(STab, (Ss + 0x10)*sizeof *STab);
   STab[Ss].Pre = 0, STab[Ss].Size = Size, STab[Ss].List = List;
   return Ss++;
}

void AddReduce(int Q, Symbol LHS, Rule RHS) {
   int I, I1; int Diff;
   for (I = 0; I < Rs; I++)
      if ((Diff = CompRd(&RList[I], Q, LHS, RHS)) >= 0) break;
   if (Rs > 0 && Diff == 0) return;
   if (Rs%0x10 == 0) RList = Reallocate(RList, (Rs + 0x10)*sizeof *RList);
   for (I1 = Rs++; I1 > I; I1--) RList[I1] = RList[I1 - 1];
   RList[I].Q = Q, RList[I].LHS = LHS, RList[I].RHS = RHS;
}

void AddShift(int Q, Symbol X, int R) {
   int I, I1; int Diff;
   for (I = 0; I < Shs; I++)
      if ((Diff = CompSh(&SList[I], Q, X, R)) >= 0) break;
   if (Shs > 0 && Diff == 0) return;
   if (Shs%0x10 == 0) SList = Reallocate(SList, (Shs + 0x10)*sizeof *SList);
   for (I1 = Shs++; I1 > I; I1--) SList[I1] = SList[I1 - 1];
   SList[I].Q = Q, SList[I].X = X, SList[I].R = R;
}

Items XTab; unsigned XMax, Xs;

Items GetItem(Symbol Pre) {
   unsigned X;
   for (X = 0; X < Xs; X++)
      if (Pre == XTab[X].Pre) break;
   if (X >= Xs) {
      if (Xs >= XMax)
         XMax += 0x10, XTab = Reallocate(XTab, XMax * sizeof *XTab);
      X = Xs++;
      XTab[X].Pre = Pre, XTab[X].Size = 0, XTab[X].List = 0;
   }
   return &XTab[X];
}

Item FormItem(Symbol LHS, Rule RHS) {
   Item It = Allocate(sizeof *It);
   It->LHS = LHS, It->Pos = It->RHS = RHS;
   return It;
}

void AddItem(Items Q, Item It) {
   int I, J, Diff;
   for (I = 0; I < Q->Size; I++) {
      Diff = CompI(Q->List[I], It);
      if (Diff == 0) return;
      if (Diff > 0) break;
   }
   if (Q->Size%0x10 == 0)
      Q->List = Reallocate(Q->List, (Q->Size + 0x10)*sizeof *Q->List);
   for (J = Q->Size++; J > I; J--) Q->List[J] = Q->List[J - 1];
   Q->List[I] = CopyI(It);
}

void Generate(Symbol Start) {
   Item It, *Its, *QBuf; int S, X, R, Q, Qs, QMax; Items QS;
   Rule StartR = Allocate(2 * sizeof *StartR);
   StartR[0] = Start, StartR[1] = 0;
   Its = Allocate(sizeof *Its), Its[0] = FormItem(0, StartR);
   STab = 0, Ss = 0; AddState(1, Its);
   XTab = 0, XMax = 0;
   QBuf = 0, QMax = 0;
   for (S = 0; S < Ss; S++) {
      unsigned E, R;
      QS = &STab[S];
      if (QS->Size > QMax)
         QMax = QS->Size, QBuf = Reallocate(QBuf, QMax * sizeof *QBuf);
      for (Qs = 0; Qs < QS->Size; Qs++) QBuf[Qs] = QS->List[Qs];
      for (Xs = 0, Q = 0; Q < Qs; Q++) {
         It = QBuf[Q];
         if (*It->Pos != 0) {
            Symbol Pre = *It->Pos++; Items IS = GetItem(Pre);
            if (IS->Size == 0) {
               if (Qs + Pre->Rules > QMax)
                  QMax = Qs + Pre->Rules,
                  QBuf = Reallocate(QBuf, QMax * sizeof *QBuf);
               for (R = 0; R < Pre->Rules; R++, Qs++)
                  QBuf[Qs] = FormItem(Pre, Pre->RList[R]);
            }
            AddItem(IS, It);
            --It->Pos;
         }
      }
      for (Q = 0; Q < Qs; Q++) {
         It = QBuf[Q];
         if (*It->Pos == 0) AddReduce(S, It->LHS, It->RHS);
      }
      for (X = 0; X < Xs; X++)
         AddShift(S, XTab[X].Pre, AddState(XTab[X].Size, XTab[X].List));
   }
   free(XTab), free(QBuf);
}

void ShowStates(void) {
   int I;
   printf("START -> <0| Q\n");
   ShowShifts();
   ShowReduces();
}

void main(void) {
   Symbol Start = Grammar();
   if (ERRORS > 0) printf("Aborted.\n"), exit(1);
   Generate(Start);
   ShowStates();
   exit(0);
}
