import scala.io.Source

// CMSC 22100, Autumn 2022, Homework 6

enum Token:
  case TLParen
  case TRParen
  case TLBrack
  case TRBrack
  case TDot
  case TColon
  case TZero
  case TArrow
  case KW_Ref
  case KW_seq
  case KW_true
  case KW_false
  case KW_unit
  case KW_if
  case KW_let
  case TVar(varName:String)
  case TExn(exnName:String)
  case TTy(typeName:String)
  case KW_raise
  case KW_try
  case KW_eq
  case KW_ref
  case TBang
  case TColonEq

enum Type:
  case TyNat
  case TyBool
  case TyUnit
  case TyFunc(tau1:Type,tau2:Type)
  case TyRef(tau1:Type)
  // the Morph type is not in the surface language
  case Morph

enum Term:
  case TmZero
  case TmTrue
  case TmFalse
  case TmUnit
  case TmIf(t1:Term,t2:Term,t3:Term)
  case TmVar(varName:String)
  case TmAbs(varName:String,tau:Type,body:Term)
  case TmApp(t1:Term,t2:Term)
  case TmExn(e:String)
  case TmRaise(t1:Term)
  case TmTry(t1:Term,t2:Term)
  case TmEq(t1:Term,t2:Term)
  case TmRef(t1:Term)
  case TmRead(t1:Term)
  case TmWrite(t1:Term,t2:Term)
  // --- syntatic sugar (surface language only)
  case TmSeq(ts:List[Term])
  case TmLet(varName:String,t1:Term,t2:Term)
  // --- intermediate forms (not in surface language)
  case UAbs(varName:String,body:Term)
  case Loc(n:Int)

import Token.*
import Type.*
import Term.*

// === miscellaneous utilities

def unparseTy(tau:Type): String = tau match {
  case TyNat => "N"
  case TyBool => "B"
  case TyUnit => "U"
  case TyFunc(tau1,tau2) => s"[$tau1->$tau2]"
  case TyRef(tau1) => s"[Ref $tau1]"
  case Morph => "Morph"
}

def unparse(t:Term): String = {
  def lp(t:Term): String = t match {
    case TmZero => "0"
    case TmTrue => "true"
    case TmFalse => "false"
    case TmUnit => "unit"
    case TmIf(t1,t2,t3) => s"(if ${lp(t1)} ${lp(t2)} ${lp(t3)})"
    case TmVar(x) => x
    case TmAbs(x,tau,b) => s"[$x:${unparseTy(tau)}.${lp(b)}]"
    case TmApp(t1,t2) =>s"(${lp(t1)} ${lp(t2)})"
    case TmExn(e) => s"E_$e"
    case TmRaise(t1) => s"(raise ${lp(t1)})"
    case TmTry(t1,t2) => s"(try ${lp(t1)} ${lp(t2)})"
    case TmEq(t1,t2) => s"(eq ${lp(t1)} ${lp(t2)})"
    case TmRef(t1) => s"(ref ${lp(t1)})"
    case TmRead(t1) => s"(!${lp(t1)})"
    case TmWrite(t1,t2) => s"(${lp(t1)}:=${lp(t2)})"
    case TmSeq(ts) => throw new Exception("seq")
    case TmLet(x,t1,t2) => s"(let $x ${lp(t1)} ${lp(t2)})"
    case UAbs(x,b) => s"[$x.${lp(b)}]"
    case Loc(n) => s"Loc_$n"
  }
  return lp(t)
}

def isV(t: Term): Boolean = t match {
  case TmZero | TmTrue | TmFalse | TmUnit 
     | TmAbs(_,_,_) | UAbs(_,_) | TmExn(_) | Loc(_) => true
  case _ => false
}

// === type environment

type TypeEnv = List[(String,Type)]

val emptyTypeEnv: TypeEnv = Nil

def lookup(x: String, gamma: TypeEnv): Option[Type] = {
  def lp(env: TypeEnv) : Option[Type] = env match {
    case Nil => None
    case (y,tau)::tl => if x==y then Some(tau) else lp(tl)
  }
  return lp(gamma)
}

def extend(x: String, tau: Type, gamma: TypeEnv): TypeEnv = (x,tau)::gamma

// === mutable memory

import scala.collection.mutable.Map

type Store = Map[Int, Term]

val emptyStore: Store = Map.empty[Int, Term]

var index = 0;

// return the newly-allocated location index, and the modified store
def malloc(v:Term, mu: Store): (Int, Store) = //throw new Exception("todo: malloc")
    mu.update(index, v)
    index +=1
    return ((index - 1), mu)
    

// read from given location in memory
// raise an exception if given location is not in the store
def read(i:Int, mu:Store): Term = //throw new Exception("todo: read")
    if(i < index)
        var term = mu.get(i)
        term match 
            case Some(t1) => t1
            case _ => throw new Exception("error read: index out of range")
    else
        throw new Exception("error read: index out of range")

// write to memory and return the modified store
// raise an exception if given location is not in the store
def write(i:Int, v:Term, mu:Store): Store = //throw new Exception("todo: write")
    if(i < index)
        mu.update(i, v)
        return mu
    else
        throw new Exception("error write: index out of range")

// build a string representation of the store
def storeString(mu:Store): String = //throw new Exception("todo: storeString")
    mu.toString()

// === scanning

def concat1(c:Char,pr:(String,List[Char])) = pr match {
  case (s,cs) => (s"${c}$s",cs)
}

def gatherChars(test:Char=>Boolean,cs:List[Char]): (String,List[Char]) = cs match {
  case Nil => ("",Nil)
  case c::tl if test(c) => concat1(c,gatherChars(test,tl))
  case _ => ("",cs)
}

def nextToken(cs: List[Char]): Option[(Token,List[Char])] = cs match {
  case Nil => None
  case '('::tl => Some(TLParen,tl)
  case ')'::tl => Some(TRParen,tl)
  case '['::tl => Some(TLBrack,tl)
  case ']'::tl => Some(TRBrack,tl)
  case '.'::tl => Some(TDot,tl)
  case ':'::'='::tl => Some(TColonEq,tl)
  case ':'::tl => Some(TColon,tl)
  case '0'::tl => Some(TZero,tl)
  case '-'::'>'::tl => Some(TArrow,tl)
  case 'R'::'e'::'f'::tl => Some(KW_Ref,tl)
  case 'E'::'_'::tl => gatherChars(_.isLetter,tl) match {
    case ("",tl) => throw new Exception("incomplete exn name E_")
    case (e,tl) => Some(TExn(e),tl)
  }
  case '!'::tl => Some(TBang,tl)
  case c::_ if c.isLower => gatherChars(_.isLower,cs) match {
    case ("seq",tl) => Some(KW_seq,tl)
    case ("true",tl) => Some(KW_true,tl)
    case ("false",tl) => Some(KW_false,tl)
    case ("unit",tl) => Some(KW_unit,tl)
    case ("if",tl) => Some(KW_if,tl)
    case ("raise",tl) => Some(KW_raise,tl)
    case ("try",tl) => Some(KW_try,tl)
    case ("eq",tl) => Some(KW_eq,tl)
    case ("ref",tl) => Some(KW_ref,tl)
    case ("let",tl) => Some(KW_let,tl)
    case (x,tl) => Some(TVar(x),tl)
  }
  case c::tl if c.isUpper => gatherChars(_.isUpper,cs) match {
    case (t,tll) => Some(TTy(t),tll)
  }
  case c::tl if c.isWhitespace => nextToken(tl)
  case _ => throw new Exception(s"scan error: ${cs.mkString}")
}

def scan(code:String): List[Token] = {
  def lp(cs:List[Char]): List[Token] = nextToken(cs) match {
    case None => Nil
    case Some(tok,cs) => tok::lp(cs)
  }
  return lp(code.toList)
}

// === parsing

def nextTy(toks:List[Token]): Option[(Type,List[Token])] = toks match {
  case Nil => None
  case TTy(t)::tl => t match {
    case "N" => Some(TyNat,tl)
    case "B" => Some(TyBool,tl)
    case "U" => Some(TyUnit,tl)
    case _ => throw new Exception(s"unknown type: $t")
  }
  case TLBrack::KW_Ref::tl => nextTy(tl) match {
    case Some(tau1,TRBrack::tl1) => Some(TyRef(tau1),tl1)
    case _ => throw new Exception("error parsing Ref type")
  }
  case TLBrack::tl => nextTy(tl) match {
    case Some(tau1,TArrow::tl1) => nextTy(tl1) match {
      case Some(tau2,TRBrack::tl2) => Some(TyFunc(tau1,tau2),tl2)
      case _ => throw new Exception("error parsing function type (1)")
    }
    case _ => throw new Exception("error parsing function type (2)")
  }
  case _ => throw new Exception(s"error parsing type ${toks.mkString}")
}

def nextTerm(toks:List[Token]): Option[(Term,List[Token])] = toks match {
  case Nil => None
  case TVar(x)::tl => Some(TmVar(x),tl)
  case KW_true::tl => Some(TmTrue,tl)
  case KW_false::tl => Some(TmFalse,tl)
  case TZero::tl => Some(TmZero,tl)
  case KW_unit::tl => Some(TmUnit,tl)
  case TLParen::KW_if::tl => nextTerm(tl) match {
    case Some(t1, tl1) => nextTerm(tl1) match {
      case Some(t2, tl2) => nextTerm(tl2) match {
        case Some(t3, TRParen::tl3) => Some(TmIf(t1,t2,t3), tl3)
        case _ => throw new Exception("if expected rparen")
      }
      case _ => throw new Exception("if ended after second subterm")
    }
    case _ => throw new Exception("if ended after first subterm")
  }
  case TLBrack::TVar(x)::TColon::tl => nextTy(tl) match {
    case Some(tau,TDot::tl1) => nextTerm(tl1) match {
      case Some(body,TRBrack::tl2) => Some(TmAbs(x,tau,body),tl2)
      case Some(_,_) => throw new Exception("parse error: missing right bracket after abs")
      case None => throw new Exception("parse error: abs ended after type")
    }
    case Some(_,_) => throw new Exception("parse error: missing dot in abstraction")
    case None => throw new Exception("parse error: abs ended after colon")
  }
  case TExn(e)::tl => Some(TmExn(e),tl)
  case TLParen::KW_raise::tl => Some(nextSub1(TmRaise(_),"raise",tl))
  case TLParen::KW_try::tl => Some(nextSub2((t1:Term,t2:Term)=>TmTry(t1,t2),"try",tl))
  case TLParen::KW_eq::tl => Some(nextSub2((t1:Term,t2:Term)=>TmEq(t1,t2),"eq",tl))
  case TLParen::KW_ref::tl => Some(nextSub1(TmRef(_),"ref",tl))
  case TLParen::TBang::tl => Some(nextSub1(TmRead(_),"read",tl))
  case TLParen::KW_seq::tl => seq(tl) match {
    case (Nil,_) => throw new Exception("empty seq")
    case (ts,tll) => Some(TmSeq(ts),tll)
  }
  case TLParen::KW_let::TVar(x)::tl => Some(nextSub2((t1:Term,t2:Term)=>TmLet(x,t1,t2),"let",tl))
  case TLParen::tl => nextTerm(tl) match {
    case Some(t1,TColonEq::tl1) => nextTerm(tl1) match {
      case Some(t2,TRParen::tl2) => Some(TmWrite(t1,t2),tl2)
      case _ => throw new Exception("parse error: expected rparen enclosing write")
    }
    case Some(t1,tl1) => nextTerm(tl1) match {
      case Some(t2,TRParen::tl2) => Some(TmApp(t1,t2),tl2)
      case _ => throw new Exception("parse error: expected rparen enclosing app")
    }
    case _ => throw new Exception(s"parse error: ${toks.mkString}")
  }
  case _ => throw new Exception(s"parse error: ${toks.mkString}")
}

def nextSub1(k:Term=>Term, name:String, toks:List[Token]): (Term,List[Token]) = nextTerm(toks) match {
  case Some(t1, TRParen::tl1) => (k(t1), tl1)
  case Some(_,_) => throw new Exception(s"parse error: expected closing paren after $name")
  case None => throw new Exception(s"parse error: $name ended unexpectedly")
}

def nextSub2(k:(Term,Term)=>Term, name:String, toks:List[Token]): (Term,List[Token]) =  nextTerm(toks) match {
  case Some(t1, tl1) => nextTerm(tl1) match {
    case Some(t2, TRParen::tl2) => (k(t1,t2),tl2)
    case Some(_,_) => throw new Exception(s"parse error: expected closing paren after $name")
    case None => throw new Exception(s"parse error: $name ended unexpectedly (2)")
  }
  case None => throw new Exception(s"parse error: $name ended unexpectedly (1)")
}

def seq(toks:List[Token]): (List[Term],List[Token]) = {
  def cons1(t:Term, p:(List[Term],List[Token])) = p match {
    case (terms,toks) => (t::terms,toks)
  }
  def lp(toks:List[Token]): (List[Term],List[Token])  = nextTerm(toks) match {
    case Some(t,TRParen::tl) => (List(t),tl)
    case Some(t,tl) => cons1(t,lp(tl))
    case None => throw new Exception("parse error: seq ended unexpectedly")
  }
  return lp(toks)
}

def parse(toks: List[Token]): Term = nextTerm(toks) match {
  case None => throw new Exception("not enough program")
  case Some(st,Nil) => st
  case Some(_,_) => throw new Exception("too much program")
}

// === type checking

// *** No type checking in HW6! ***

// === rewriting

var freshVarSeed = 0
def freshVarName(): String =
  val name:String = s"_v$freshVarSeed"
  freshVarSeed += 1
  return name

// rewrite TmAbs to UAbs
// rewrite TmSeq as per TaPL 11.3 ("An alternative way of formalizing...")
// rewrite TmLet as per TaPL 11.5.1, but discard the type
def rewrite(t: Term): Term = //throw new Exception("todo: rewrite")
    t match
        case TmZero => TmZero
        case TmTrue => TmTrue
        case TmFalse => TmFalse
        case TmUnit => TmUnit
        case TmIf(t1, t2, t3) => TmIf(rewrite(t1), rewrite(t2), rewrite(t3))
        case TmVar(varName) => TmVar(varName)
        case TmAbs(varName, ty, body) => varName.split(" ").toList match
            case head :: Nil => UAbs(varName.mkString, rewrite(body))
            case head :: tail => UAbs(head.toString, rewrite(TmAbs(tail.mkString, ty, body)))
            case _ => throw new Exception("rewrite error: TmAbs ended unexpectedly")
        case TmApp(t1, t2) => TmApp(rewrite(t1), rewrite(t2))
        case TmExn(exception) => TmExn(exception)
        case TmRaise(t1) => TmRaise(rewrite(t1))
        case TmTry(t1, t2) => TmTry(rewrite(t1), rewrite(t2))
        case TmEq(t1, t2) => TmEq(rewrite(t1), rewrite(t2))
        case TmRef(t1) => TmRef(rewrite(t1))
        case TmRead(t1) => TmRead(rewrite(t1))
        case TmWrite(t1, t2) => TmWrite(rewrite(t1), rewrite(t2))
        //syntactic sugar
        case TmSeq(ts) => ts match  
            case Nil => throw new Exception("error rewrite: sequence list empty")
            case t1 :: Nil => rewrite(t1)
            case t1 :: t2 :: tail =>
                rewrite(TmSeq(TmApp(UAbs(freshVarName(), t2), t1) :: tail))
        case TmLet(varName, t1, t2) =>
            rewrite(TmApp(UAbs(varName, t2), t1))
        //case TmAbs(varName, tau, body) => UAbs(varName, body)
        case Loc(n) => Loc(n)
        case _ => throw new Exception("error rewrite: invalid inputted term")


// === evaluation

def fv(t:Term): Set[String] = t match {
  case TmZero => Set()
  case TmTrue => Set()
  case TmFalse => Set()
  case TmUnit => Set()
  case TmIf(t1,t2,t3) => fv(t1)++fv(t2)++fv(t3)
  case TmVar(x) => Set(x)
  case TmAbs(x,tau,b) => throw new Exception("fv: typed abstraction (should have been rewritten)")
  case TmApp(t1,t2) => fv(t1)++fv(t2)
  case TmExn(_) => Set()
  case TmRaise(t1) => fv(t1)
  case TmTry(t1,t2) => fv(t1)++fv(t2)
  case TmEq(t1,t2) => fv(t1)++fv(t2)
  case TmRef(t1) => fv(t1)
  case TmRead(t1) => fv(t1)
  case TmWrite(t1,t2) => fv(t1)++fv(t2)
  case TmSeq(_) => throw new Exception("fv: seq form (should have been rewritten)")
  case TmLet(_,_,_) => throw new Exception("fv: let form (should have been rewritten)")
  case UAbs(x,b) => fv(b)-x
  case Loc(_) => Set()
}

def subst(x:String,s:Term,t:Term): Term = t match {
  case TmZero | TmTrue | TmFalse | TmUnit => t
  case TmIf(t1,t2,t3) => TmIf(subst(x,s,t1),subst(x,s,t2),subst(x,s,t3))
  case TmVar(y) if x==y => s
  case TmVar(y) => t
  case UAbs(y,_) if y==x => t
  case TmAbs(_,_,_) => throw new Exception("subst: typed abstraction (should have been rewritten)")
  case TmApp(t1,t2) => TmApp(subst(x,s,t1),subst(x,s,t2))
  case TmExn(_) => t
  case TmRaise(t1) => TmRaise(subst(x,s,t1))
  case TmTry(t1,t2) => TmTry(subst(x,s,t1),subst(x,s,t2))
  case TmEq(t1,t2) => TmEq(subst(x,s,t1),subst(x,s,t2))
  case TmRef(t1) => TmRef(subst(x,s,t1))
  case TmRead(t1) => TmRead(subst(x,s,t1))
  case TmWrite(t1,t2) => TmWrite(subst(x,s,t1),subst(x,s,t2))
  case TmSeq(_) => throw new Exception("subst: seq form (should have been rewritten)")
  case TmLet(_,_,_) => throw new Exception("subst: let form (should have been rewritten)")
  case UAbs(y,b) => fv(s) contains y match {
    case true => throw new Exception("this case should not arise in a well-typed progran under CBV")
    case false => UAbs(y,subst(x,s,b))
  }
  case Loc(_) => t
}

def getIntStore(t:Term, mu:Store): Int =
  var vals = mu.toList
  var i = 0
  var ans = -1;
  while(i < vals.length)
  {
    if(t == vals(i)(1))
    {
      ans = i
    }
    i += 1
  }
  if(ans == -1)
  {
    throw new Exception("error getIntoStore: term not in store")
  }
  else
  {
    return ans
  }

def step(t: Term, mu: Store): Option[(Term,Store)] = 
  //print("in step\n")
  //println("new term: " + t)
  t match 
  {
  //TmAbs should be modified to UAbs
  case TmAbs(_,_,_) => throw new Exception("step: typed abstraction (should have been rewritten)")
  case TmSeq(_) => throw new Exception("step: seq form (should have been rewritten)")
  case TmLet(_,_,_) => throw new Exception("step: let form (should have been rewritten)")

  //case _ if isV(t) => None
  case _ if isV(t) => Some(t, mu)

  case TmVar(varName) => Some(t, mu)

  case TmApp(t1, t2) =>
    //print("IN APPLICATION\n")
    if(!isV(t1))
    {
      step(t1, mu) match 
        case Some(term, memory) => step(TmApp(term, t2), write(getIntStore(t, mu), TmApp(term, t2), mu))
        case _ => None
    }
    else if(!isV(t2))
    {
      step(t2, mu) match
        case Some(term, memory) => step(TmApp(t1, term), write(getIntStore(t, mu), TmApp(t1, term), mu))
        case _ => None
    }
    else
    {
      t1 match
        case UAbs(varName, body) => 
          //println("in uabs of the tmapp case")
          step(subst(varName, t2, body), mu)
        case _ => None
    }

  case TmIf(t1, t2, t3) =>
    //print("IN TERM IF\n")
    t1 match
      case TmRaise(term) => 
        term match
          case TmExn(exception) => Some(term, mu)
          case _ => throw new Exception("step error: if raise case invalid input")
      case TmTrue => step(t2, mu)
      case TmFalse => step(t3, mu)
      case _ => 
        step(t1, mu) match
          case Some(term1, memory) => step(TmIf(term1, t2, t3), memory)
          case _ => None

  case TmEq(t1, t2) => 
  //print("IN TERM EQ\n")
   t1 match
    //case TmRaise(TmExn(exception)) => Some(t1, mu)
    //case TmExn(exception) => Some(TmRaise(t1), mu)
    case _ if isV(t1) => 
      t2 match 
        //case TmRaise(TmExn(exception)) => Some(t2, mu)
        //case TmExn(exception) => Some(TmRaise(t2), mu)
        case _ if isV(t2) => 
          if(t1==t2)
          {
            //println("in equal true")
            Some(TmTrue,mu)
          }
          else
          {
            //println("in equal false")
            Some(TmFalse,mu)
          }
        case _ => step(t2, mu) match
          case Some(term2, memory) => step(TmEq(t1, term2), memory)
          case _ => None
    case _ => step(t1,mu) match
      case Some(term1, memory) => step(TmEq(term1, t2), memory)
      case _ => None



  case TmRef(t1) =>
    //print("IN TERM REF\n")
    t1 match
      //case TmRaise(TmExn(exception)) => Some(t1, mu)
      case TmExn(exception) => Some(TmRaise(t1), mu)
      case _ if isV(t1) => //if t1 is a value
        malloc(t1, mu) match
          case (integer, store) => Some(Loc(integer), store)
      case _ => step(t1, mu) match
        case Some(term, mem) => step(TmRef(term), write(getIntStore(t, mu), TmRef(term), mu))
        case _ => None


  case TmRead(t1) =>
    //print("IN TERM READ\n")
    t1 match
      //case TmRaise(TmExn(exception)) => Some(t1, mu)
      case TmExn(exception) => Some(TmRaise(t1), mu)
      case Loc(i) => Some(read(i, mu), mu)
      case _ => step(t1, mu) match
        case Some(term, memory) => Some(TmRead(term), write(getIntStore(t1, mu), TmRead(term), mu))
        case _ => None

  case TmWrite(t1, t2) =>
    //print("IN TERM WRITE\n")
    t1 match 
      case Loc(integer) => 
        t2 match 
          //if t1 = Loc(int) and t2 = a value
          case _ if isV(t2) => Some(TmUnit, write(integer, t2, mu))
          case _ => step(t2, mu) match
            case Some(term, memory) => step(TmWrite(t1, term), write(getIntStore(t, mu), TmWrite(t1, term), mu))
            case _ => None
      //case TmRaise(TmExn(exception)) => Some(t1, mu)
      case TmExn(exception) => Some(TmRaise(t1), mu)
      case _ if isV(t1) => 
        t2 match 
          //case TmRaise(TmExn(exception)) => Some(t2, mu)
          case TmExn(exception) => Some(TmRaise(t2), mu)
          case _ if isV(t2) => 
            Some(TmUnit, write(getIntStore(t1, mu), t2, mu))
          case _ => step(t2, mu) match
            case Some(term, memory) => step(TmWrite(t1, term), write(getIntStore(t, mu), TmWrite(t1, term), mu))
            case _ => None
      case _ => //t1 is not a value
        step(t1, mu) match
          case Some(term, memory) => step(TmWrite(term, t2), write(getIntStore(t, mu), TmWrite(term, t2), mu))
          case _ => None
  case TmTry(t1, t2) => 
    //print("IN TERM TRY\n")
    //println(t1)
    t1 match 
      case TmExn(exception) => 
        //println("term exception")
        Some(t1, mu)
      case TmRaise(TmExn(exception)) =>
        Some(TmApp(t2, TmExn(exception)), mu)
      case _ if isV(t1) => Some(t1, mu)
      case _ => step(t1, mu) match
          case Some(term, memory) => step(TmTry(term, t2), write(getIntStore(t, mu), TmTry(term, t2), mu))
          case _ => None

  case _ =>  
    //print("TERM DID NOT MATCH TO CASE\n")
    throw new Exception("todo: step (all the other cases)")

}

// ===== interpreter

def steps(tm: Term): List[(Term,Store)] = {
  //println("in steps: " + tm)
  def lp(t:Term, mu:Store): List[(Term,Store)] = 
    //println("in lp: " + t)
    step(t,mu) match {
      case None => 
        List((t,mu))
      case Some(t1,mu1) => t1 match
        case _ if isV(t1) =>
          List((t1,mu))
        case _ => (t,mu)::lp(t1,mu1)
    }
  //println("ouput of lp is: " + lp.toString())
  return lp(tm,emptyStore)
}

@main def interpret(codeOrFilename: String): Unit = {
  var code = codeOrFilename
  try {
    code = Source.fromFile(codeOrFilename).getLines.mkString
  } catch {
    case e: java.io.FileNotFoundException => "pass" // do nothing
  }
  println(code)
  println()
  val toks = scan(code)
  val term = parse(toks)
  //print(term)
  val rtrm = rewrite(term)
  println("after rewriting:")
  println(unparse(rtrm))
  println()
  println("evaluation:")
  try {
    //print("before stps")
    val stps = steps(rtrm)
    //println("after stps")
    for s <- stps do {
      println(unparse(s(0)))
    }
    val lastStore = stps.last(1)
    println(s"store:${storeString(lastStore)}")
    //println("made it to the end of the try")
  } catch {
    case e => println("exception during evaluation\n$e")
  }
}

@main def test_malloc() = 
    print(s"testing malloc: ${malloc(TmFalse, emptyStore)}\n")
    print(s"testing malloc: ${malloc(TmTrue, emptyStore)}\n")
    print(s"testing malloc: ${malloc(TmZero, emptyStore)}\n")


@main def test_read() = 
    malloc(TmTrue, emptyStore)
    malloc(TmFalse, emptyStore)
    malloc(TmZero, emptyStore)

    print(s"testing read: ${read(2, emptyStore)}\n")
    print(s"testing read: ${read(3, emptyStore)}\n")

@main def test_write() = 
    malloc(TmTrue, emptyStore)
    malloc(TmFalse, emptyStore)
    malloc(TmZero, emptyStore)

    print(s"testing write: ${write(1, TmTrue, emptyStore)}\n")
    print(s"testing write: ${write(4, TmTrue, emptyStore)}\n")

@main def test_storeString() = 
    malloc(TmTrue, emptyStore)
    malloc(TmFalse, emptyStore)
    malloc(TmZero, emptyStore)
    
    print(s"testing storeString: ${storeString(emptyStore)}")

@main def test_getIntStore() = 
    malloc(TmTrue, emptyStore)
    malloc(TmFalse, emptyStore)
    malloc(TmZero, emptyStore)

    print(s"testing GetIntStore: ${getIntStore(TmTrue, emptyStore)}\n")
    print(s"testing GetIntStore: ${getIntStore(TmFalse, emptyStore)}\n")
    print(s"testing GetIntStore: ${getIntStore(TmZero, emptyStore)}\n")