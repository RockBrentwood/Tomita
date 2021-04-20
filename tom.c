#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

/* Some compilers (such as gcc on the SUN) require this:
#define realloc(X, N) ((X) == 0? malloc(N): realloc((X), (N)))
 */

/* An implementation of the Tomita Parsing Algorithm, using LR(0) parsing.
   Suggested modifications:
    + Generalize the LR(k) parsing automaton to a form suitable for handling
      grammars with rules of the form:
                     Non-Terminal = Regular-Expresion

    + Extend the LR(0) parser to LALR(1).

    + Add in attributes and error-handling.
 */

/* LR(0) PARSER GENERATOR */
/* Input format derived from the syntax:
   Grammar = Rule+.
   Rule = "*" ID "." |
          ID "." |
          ID "=" ID* ("|" ID*)* "." |
          ID ":" ID+ ".".
 */

/* THE SCANNER */
typedef enum { EndT, StarT, ColonT, EqualT, BarT, IdenT, DotT } Lexical;

char *LastW;

#define MAX_CHAR 0x400
static char ChArr[MAX_CHAR], *ChP = ChArr;

static int LINE = 1, ERRORS = 0;
#define MAX_ERRORS 25

FILE *InF;

static int GET(void) {
   int Ch = fgetc(InF);
   if (Ch == '\n') LINE++;
   return Ch;
}

static void UNGET(int Ch) {
   if (Ch == '\n') --LINE;
   ungetc(Ch, InF);
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
      case EOF: return EndT;
      case '|': return BarT;
      case '*': return StarT;
      case ':': return ColonT;
      case '=': return EqualT;
      case '.': return DotT;
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
      return IdenT;
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
      return IdenT;
   } else {
      ERROR("extra character %c", Ch); return EndT;
   }
}

/* DATA STRUCTURES */
typedef unsigned char byte;
typedef struct Symbol *Symbol;
typedef Symbol *Rule;
struct Symbol {
   char *Name; byte Defined, Literal; unsigned Index;
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

Symbol LookUp(char *S, byte Literal) {
   static int LABEL = 0;
   Symbol Sym; byte H;
   for (H = Hash(S), Sym = HashTab[H]; Sym != 0; Sym = Sym->Next)
      if (Sym->Literal == Literal && strcmp(Sym->Name, S) == 0) return Sym;
   Sym = (Symbol)Allocate(sizeof *Sym);
   Sym->Name = CopyS(S), Sym->Literal = Literal;
   Sym->Index = LABEL++, Sym->Defined = 0;
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
      case EndT: return Start;
      case IdenT: goto EQUAL;
      case DotT: L = LEX(); goto START;
      case ColonT: case EqualT:
	 ERROR("Missing left-hand side of rule, or '.' from previous rule.");
      goto FLUSH;
      case StarT:
         L = LEX();
         if (L != IdenT) ERROR("Missing symbol after '*'.");
         else {
            Start = LookUp(LastW, 0);
            if (SawStart++ > 0) ERROR("Start symbol redefined.");
            L = LEX();
         }
      goto FLUSH;
      default: case BarT: ERROR("Corrupt rule."); goto FLUSH;
   }
FLUSH:
   for (; L != DotT && L != EndT; L = LEX()) ;
goto END;
EQUAL:
   LHS = LookUp(LastW, 0); if (Start == 0) Start = LHS;
   L = LEX(); LHS->Defined = 1;
   if (L == DotT) {
      SymP = SymBuf; *SymP++ = LHS, *SymP++ = 0;
      InsertR(LookUp(LHS->Name, 1));
   } else if (L == ColonT) {
      SymP = SymBuf; *SymP++ = LHS, *SymP++ = 0;
      for (L = LEX(); L == IdenT; L = LEX()) InsertR(LookUp(LastW, 1));
   } else if (L == EqualT) {
      do {
         L = LEX();
         for (SymP = SymBuf; L == IdenT && SymP < SymBuf + MAX_SYM; L = LEX())
            *SymP++ = LookUp(LastW, 0);
         if (SymP >= SymBuf + MAX_SYM) printf("Large rule.\n"), exit(1);
         *SymP++ = 0; InsertR(LHS);
      } while (L == BarT);
   } else {
      ERROR("Missing '=' or ':'."); goto FLUSH;
   }
END:
   if (L == EndT) { ERROR("Missing '.'"); return Start; }
   if (L == DotT) L = LEX();
goto START;
}

void Check(void) {
   Symbol S;
   for (S = FirstB; S != 0; S = S->Tail)
      if (!S->Defined && !S->Literal) ERROR("%s undefined.\n", S->Name);
   if (ERRORS > 0) printf("Aborted.\n"), exit(1);
}

typedef struct Item { Symbol LHS, *RHS, *Pos; } *Item;
typedef struct Items { Symbol Pre; int Size; Item *List; } *Items;

Items STab; int Ss;

Item CopyI(Item A) {
   Item B = Allocate(sizeof *B);
   *B = *A;
   return B;
}

int CompI(Item A, Item B) {
   int Diff; Symbol *AP, *BP;
   Diff =
      A->LHS == 0? (B->LHS == 0? 0: -1):
      B->LHS == 0? +1: A->LHS->Index - B->LHS->Index;
   if (Diff != 0) return Diff;
   Diff = (A->Pos - A->RHS) - (B->Pos - B->RHS); if (Diff != 0) return Diff;
   for (AP = A->RHS, BP = B->RHS; *AP != 0 && *BP != 0; AP++, BP++)
      if ((Diff = (*AP)->Index - (*BP)->Index) != 0) break;
   return *AP == 0? (*BP == 0? 0: -1): *BP == 0? +1: Diff;
}

typedef struct Reduce *Reduce;
typedef struct Shift *Shift;
typedef struct State *State;

struct Reduce { Symbol LHS, *RHS; };
struct Shift { Symbol X; int Q; };
struct State {
   byte Final; unsigned Es, Rs, Ss;
   Symbol *EList; Reduce RList; Shift SList;
};
State SList;

State Next(State Q, Symbol Sym) {
   int S; Shift Sh;
   for (S = 0; S < Q->Ss; S++) {
      Sh = &Q->SList[S];
      if (Sh->X == Sym) return &SList[Sh->Q];
   }
   return 0;
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
   if ((Ss&7) == 0)
      STab = Reallocate(STab, (Ss + 8) * sizeof *STab),
      SList = Reallocate(SList, (Ss + 8) * sizeof *SList);
   STab[Ss].Pre = 0, STab[Ss].Size = Size, STab[Ss].List = List;
   return Ss++;
}

Items XTab; unsigned XMax, Xs;

Items GetItem(Symbol Pre) {
   unsigned X;
   for (X = 0; X < Xs; X++)
      if (Pre == XTab[X].Pre) break;
   if (X >= Xs) {
      if (Xs >= XMax) XMax += 8, XTab = Reallocate(XTab, XMax * sizeof *XTab);
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
   if ((Q->Size&3) == 0)
      Q->List = Reallocate(Q->List, (Q->Size + 4) * sizeof *Q->List);
   for (J = Q->Size++; J > I; J--) Q->List[J] = Q->List[J - 1];
   Q->List[I] = CopyI(It);
}

void MakeState(State S, byte Final, unsigned Es, unsigned Rs, unsigned Ss) {
   S->Final = Final;
   S->Es = Es, S->EList = Es == 0? 0: Allocate(Es * sizeof *S->EList),
   S->Rs = Rs, S->RList = Rs == 0? 0: Allocate(Rs * sizeof *S->RList),
   S->Ss = Ss, S->SList = Ss == 0? 0: Allocate(Ss * sizeof *S->SList);
}

void Generate(Symbol Start) {
   Item It, *Its, *QBuf; int S, X, R, Q, Qs, QMax; Items QS;
   Rule StartR = Allocate(2 * sizeof *StartR);
   StartR[0] = Start, StartR[1] = 0;
   Its = Allocate(sizeof *Its), Its[0] = FormItem(0, StartR);
   SList = 0, STab = 0, Ss = 0; AddState(1, Its);
   XTab = 0, XMax = 0;
   QBuf = 0, QMax = 0;
   for (S = 0; S < Ss; S++) {
      unsigned ERs, RRs, E, R; byte Final;
      QS = &STab[S];
      if (QS->Size > QMax)
         QMax = QS->Size, QBuf = Reallocate(QBuf, QMax * sizeof *QBuf);
      for (Qs = 0; Qs < QS->Size; Qs++) QBuf[Qs] = QS->List[Qs];
      for (ERs = RRs = 0, Final = 0, Xs = 0, Q = 0; Q < Qs; Q++) {
         It = QBuf[Q];
         if (*It->Pos == 0) {
            if (It->LHS == 0) Final++;
            else if (*It->RHS == 0) ERs++; else RRs++;
         } else {
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
      MakeState(&SList[S], Final, ERs, RRs, Xs);
      for (E = R = 0, Q = 0; Q < Qs; Q++) {
         It = QBuf[Q];
         if (*It->Pos != 0 || It->LHS == 0) continue;
         if (*It->RHS == 0) SList[S].EList[E++] = It->LHS;
         else {
            Reduce Rd = &SList[S].RList[R++];
            Rd->LHS = It->LHS, Rd->RHS = It->RHS;
         }
      }
      for (X = 0; X < Xs; X++) {
         Shift Sh = &SList[S].SList[X];
         Sh->X = XTab[X].Pre, Sh->Q = AddState(XTab[X].Size, XTab[X].List);
      }
   }
   free(XTab), free(QBuf);
}

void SHOW_STATES(void) {
   unsigned S; State St; Rule R; int I;
   for (S = 0; S < Ss; S++) {
      St = &SList[S];
      printf("%d:\n", S);
      if (St->Final) printf("\taccept\n");
      for (I = 0; I < St->Es; I++)
         printf("\t[%s -> 1]\n", St->EList[I]->Name);
      for (I = 0; I < St->Rs; I++) {
         printf("\t[%s ->", St->RList[I].LHS->Name);
         for (R = St->RList[I].RHS; *R != 0; R++) printf(" %s", (*R)->Name);
         printf("]\n");
      }
      for (I = 0; I < St->Ss; I++)
         printf("\t%s => %d\n", St->SList[I].X->Name, St->SList[I].Q);
   }
}

/* TOMITA PARSER */
unsigned Position;
Symbol GetC(void) {
   int Ch; Symbol Sym;
   do Ch = GET(); while (Ch == ' ' || Ch == '\t');
   if (Ch == '\n' || Ch == EOF) return 0;
   for (LastW = ChP; Ch != EOF && !isspace(Ch); ChP++) {
      if (ChP - ChArr == MAX_CHAR)
         printf("Out of character space.\n"), exit(1);
      *ChP = Ch, Ch = GET();
   }
   if (Ch != EOF) UNGET(Ch);
   if (ChP - ChArr == MAX_CHAR) printf("Out of character space.\n"), exit(1);
   *ChP++ = '\0';
   Position++;
   return Sym = LookUp(LastW, 1);
}

typedef struct Node *Node;
typedef struct Subnode *Subnode;
struct Node { Symbol Sym; unsigned Start, Size, Subs; Subnode *Sub; };
struct Subnode { unsigned Size, Cur, Links; Subnode Next; };
Node NodeTab; unsigned NodeE, NodeP;

void SHOW_NODE(unsigned N) {
   Node Nd = &NodeTab[N];
   if (Nd->Sym->Literal) printf(" \"%s\"", Nd->Sym->Name);
   else printf(" %s_%d_%d", Nd->Sym->Name, Nd->Start, Nd->Start + Nd->Size);
}

void SHOW_FOREST(void) {
   int N, S; Node Nd; Subnode P;
   for (N = 0; N < NodeE; N++) {
      Nd = &NodeTab[N];
      if (Nd->Sym->Literal) continue;
      SHOW_NODE(N);
      if (Nd->Subs > 0) {
         P = Nd->Sub[0];
         if (NodeTab[P->Cur].Sym->Literal) putchar(':'); else printf(" =");
      }
      for (S = 0; S < Nd->Subs; S++) {
         if (S > 0) printf(" |");
         for (P = Nd->Sub[S]; P != 0; P = P->Next) SHOW_NODE(P->Cur);
      }
      printf(".\n");
   }
}

int EqualS(Subnode A, Subnode B) {
   for (; A != 0 && B != 0; A = A->Next, B = B->Next)
      if (A->Size != B->Size || A->Cur != B->Cur) return 0;
   return A == B;
}

void FreeSub(Subnode A) {
   Subnode Next;
   for (; A != 0; A = Next) {
      Next = A->Next; free(A);
      if (Next != 0 && --Next->Links > 0) break;
   }
}

unsigned AddSub(Symbol L, Subnode P) {
   unsigned N, S, Size; Node Nd;
   Size = L->Literal? 1: (P == 0)? 0: P->Size;
   for (N = NodeP; N < NodeE; N++) {
      Nd = &NodeTab[N];
      if (Nd->Sym == L && Nd->Size == Size) break;
   }
   if (N >= NodeE) {
      if ((NodeE&7) == 0)
         NodeTab = Reallocate(NodeTab, (NodeE + 8) * sizeof *NodeTab);
      N = NodeE++, Nd = &NodeTab[N];
      Nd->Sym = L, Nd->Size = Size;
      Nd->Start =
         L->Literal? Position - 1:
         (P == 0)?   Position:
                     NodeTab[P->Cur].Start;
      Nd->Subs = 0, Nd->Sub = 0;
   }
   if (!L->Literal) {
      for (S = 0; S < Nd->Subs; S++)
         if (EqualS(Nd->Sub[S], P)) break;
      if (S >= Nd->Subs) {
         if ((Nd->Subs&3) == 0)
            Nd->Sub = Reallocate(Nd->Sub, (Nd->Subs + 4) * sizeof *Nd->Sub);
         Nd->Sub[Nd->Subs++] = P;
      } else FreeSub(P);
   }
   return N;
}

typedef struct ZNode *ZNode;
typedef struct Vertex *Vertex;
struct ZNode { unsigned Val; unsigned Size, *List; };
struct Vertex { State Val; unsigned Start; unsigned Size; ZNode *List; };
Vertex VertTab; unsigned VertE, VertP;

void SHOW_W(unsigned W) {
   Vertex V = &VertTab[W];
   printf(" v_%d_%d", V->Start, V->Val - SList);
}

void SHOW_STACK(void) {
   int W, W1, Z; Vertex V; ZNode N;
   for (W = 0; W < VertE; W++) {
      V = &VertTab[W];
      SHOW_W(W);
      if (V->Size > 0) printf(" <=\t"); else putchar('\n');
      for (Z = 0; Z < V->Size; Z++) {
         if (Z > 0) putchar('\t'), putchar('\t');
         N = V->List[Z];
         printf(" ["), SHOW_NODE(N->Val), printf(" ] <=");
         for (W1 = 0; W1 < N->Size; W1++) SHOW_W(N->List[W1]);
         putchar('\n');
      }
   }
}

typedef struct RRed *RRed;
struct RRed { ZNode Z; Symbol LHS; Rule RHS; };
RRed REDS; unsigned RP, RE;
void AddRed(ZNode Z, Symbol LHS, Rule RHS) {
   if ((RE&7) == 0) REDS = Reallocate(REDS, (RE + 8)*sizeof *REDS);
   REDS[RE].Z = Z, REDS[RE].LHS = LHS, REDS[RE].RHS = RHS;
   RE++;
}

typedef struct ERed *ERed;
struct ERed { unsigned W; Symbol LHS; };
ERed EREDS; unsigned EP, EE;
void AddERed(unsigned W, Symbol LHS) {
   if ((EE&7) == 0) EREDS = Reallocate(EREDS, (EE + 8)*sizeof *EREDS);
   EREDS[EE].W = W, EREDS[EE].LHS = LHS;
   EE++;
}

unsigned AddQ(State S) {
   unsigned V; Vertex W; int E;
   for (V = VertP; V < VertE; V++) {
      W = &VertTab[V];
      if (W->Val == S) return V;
   }
   if ((VertE&7) == 0)
      VertTab = Reallocate(VertTab, (VertE + 8)*sizeof *VertTab);
   W = &VertTab[VertE];
   W->Val = S, W->Start = Position, W->Size = 0, W->List = 0;
   for (E = 0; E < S->Es; E++) AddERed(VertE, S->EList[E]);
   return VertE++;
}

void AddN(unsigned N, unsigned W) {
   Node Nd = &NodeTab[N]; State S = Next(VertTab[W].Val, Nd->Sym);
   Vertex W1; unsigned Z; ZNode Z1; int I;
   if (S == 0) return;
   W1 = &VertTab[AddQ(S)];
   for (Z = 0; Z < W1->Size; Z++) {
      Z1 = W1->List[Z];
      if (Z1->Val == N) break;
   }
   if (Z >= W1->Size) {
      Reduce Rd; int R;
      if ((W1->Size&3) == 0)
         W1->List = Reallocate(W1->List, (W1->Size + 4) * sizeof *W1->List);
      Z = W1->Size++;
      W1->List[Z] = Z1 = Allocate(sizeof *Z1);
      Z1->Val = N, Z1->Size = 0, Z1->List = 0;
      for (R = 0; R < S->Rs; R++)
         Rd = &S->RList[R], AddRed(Z1, Rd->LHS, Rd->RHS);
   }
   for (I = 0; I < Z1->Size; I++)
      if (Z1->List[I] == W) break;
   if (I >= Z1->Size) {
      if ((Z1->Size&3) == 0)
         Z1->List = Reallocate(Z1->List, (Z1->Size + 4)*sizeof *Z1->List);
      I = Z1->Size++;
      Z1->List[I] = W;
   }
}

typedef struct Path *Path;
struct Path { ZNode Z; Subnode P; };
Path PathTab; unsigned PathE, PathP;

void AddLink(ZNode Z, Subnode P) {
   Path PP; unsigned N = Z->Val; Node Nd = &NodeTab[N];
   Subnode NewP = Allocate(sizeof *NewP);
   NewP->Size = Nd->Size; if (P != 0) NewP->Size += P->Size, P->Links++;
   NewP->Cur = N, NewP->Next = P, NewP->Links = 0;
   P = NewP;
   if ((PathE&7) == 0)
      PathTab = Reallocate(PathTab, (PathE + 8) * sizeof *PathTab);
   PP = &PathTab[PathE++];
   PP->Z = Z, PP->P = P;
}

void Reduce1(ZNode Z, Symbol L, Rule R) {
   unsigned PE, W, X; Path PP; Subnode P; Vertex V;
   PathTab = 0, PathE = PathP = 0;
   AddLink(Z, 0);
   for (R++; *R != 0; R++)
      for (PE = PathE; PathP < PE; PathP++) {
         PP = &PathTab[PathP], Z = PP->Z, P = PP->P;
         for (W = 0; W < Z->Size; W++) {
            V = &VertTab[Z->List[W]];
            for (X = 0; X < V->Size; X++) AddLink(V->List[X], P);
         }
      }
   for (; PathP < PathE; PathP++) {
      unsigned N, W; Vertex V; Node Nd;
      PP = &PathTab[PathP], Z = PP->Z, P = PP->P; Nd = &NodeTab[Z->Val];
      N = AddSub(L, P);
      for (W = 0; W < Z->Size; W++) AddN(N, Z->List[W]);
   }
   free(PathTab), PathP = PathE = 0;
}

Node Parse(void) {
   Symbol Word; unsigned C, VP, N, W; Subnode P;
   AddQ(&SList[0]);
   while (1) {
   /* REDUCE */
      while (EP < EE || RP < RE) {
         for (; RP < RE; RP++) Reduce1(REDS[RP].Z, REDS[RP].LHS, REDS[RP].RHS);
         for (; EP < EE; EP++) AddN(AddSub(EREDS[EP].LHS, 0), EREDS[EP].W);
      }
   /* SHIFT */
      Word = GetC();
      if (Word == 0) break;
      printf(" %s", Word->Name);
      P = Allocate(sizeof *P);
      P->Size = 1, P->Cur = AddSub(Word, 0), P->Next = 0, P->Links = 0;
      VP = VertP, VertP = VertE; NodeP = NodeE;
      if (Word->Rules == 0) { /* Treat the word as a new word. */
         Symbol S;
         for (S = FirstB; S != 0; S = S->Tail) {
            if (S->Rules > 0) continue;
            N = AddSub(S, P);
            for (W = VP; W < VertP; W++) AddN(N, W);
         }
      } else for (C = 0; C < Word->Rules; C++) {
         N = AddSub(*Word->RList[C], P);
         for (W = VP; W < VertP; W++) AddN(N, W);
      }
   }
/* ACCEPT */
   putchar('\n');
   for (W = VertP; W < VertE; W++) {
      Vertex V = &VertTab[W];
      if (V->Val->Final) return &NodeTab[V->List[0]->Val];
   }
   return 0;
}

void SetTables(void) {
   NodeTab = 0, NodeE = 0; Position = 0;
   VertTab = 0, VertE = 0;
   PathTab = 0, PathE = 0;
   REDS = 0, RP = RE = 0;
   EREDS = 0, EP = EE = 0;
   NodeP = NodeE; VertP = VertE;
}

void FreeTables(void) {
   int N, S, W, Z; Vertex V; ZNode ZN; Node Nd;
   for (N = 0; N < NodeE; N++) {
      Nd = &NodeTab[N];
      for (S = 0; S < Nd->Subs; S++) FreeSub(Nd->Sub[S]);
      free(Nd->Sub);
   }
   free(NodeTab), NodeTab = 0, NodeE = NodeP = 0;
   for (W = 0; W < VertE; W++) {
      V = &VertTab[W];
      for (Z = 0; Z < V->Size; Z++) {
         ZN = V->List[Z]; free(ZN->List); free(ZN);
      }
      free(V->List);
   }
   free(VertTab), VertTab = 0, VertE = VertP = 0;
   free(REDS), RE = RP = 0;
   free(EREDS), EE = EP = 0;
}

void main(int AC, char **AV) {
   Symbol Start; Node Nd; int Arg; char *AP;
   int DoC = 0, DoS = 0, DoH = 0;
   for (Arg = 1; Arg < AC; Arg++) {
      AP = AV[Arg];
      if (*AP++ != '-') break;
      for (; *AP != '\0'; AP++) switch (*AP) {
         case 's': DoS++; break;
         case 'c': DoC++; break;
         default: DoH++; break;
      }
   }
   if (DoH || Arg >= AC) {
      printf(
         "Usage: tom -sc? grammar\n"
         "    -c ...... display parsing table\n"
         "    -s ...... display parsing stack\n"
         "    -h/-? ... print this list\n"
      );
      exit(1);
   }
   InF = fopen(AV[Arg], "r");
   if (InF == 0) fprintf(stderr, "Cannot open %s.\n", AV[Arg]), exit(1);
   Start = Grammar();
   fclose(InF);
   Check();
   Generate(Start);
   if (DoC) SHOW_STATES();
   InF = stdin;
   while (1) {
      SetTables();
      Nd = Parse();
      if (Position == 0) break;
      printf("\nParse Forest:\n");
      if (Nd != 0) {
         putchar('*'); SHOW_NODE(Nd - NodeTab); printf(".\n");
      }
      SHOW_FOREST();
      if (DoS) {
         printf("\nParse Stack:\n"), SHOW_STACK();
      }
      FreeTables();
   }
   exit(0);
}
