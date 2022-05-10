#pragma once

#include <cstdint>
#include <string_view>
#include <variant>
#include <vector>

#include "../common.hpp"

struct Decl;
struct Func;

// Expr Ast
struct Expr {
  // 表达式类型
  enum Tag {
#include "op.inc" // Binary
    Call,
    Index,
    IntConst
  } tag;
  
  i32 result;  // 保存解析出来的结果，typeck阶段计算它，后续用它
};

// 操作符保存在Expr::tag中
struct Binary : Expr {
  DEFINE_CLASSOF(Expr, Tag::Add <= p->tag && p->tag <= Tag::Or);
  Expr *lhs;
  Expr *rhs;
};

// 在 Parser SDT 中实例化
struct Index : Expr {
  DEFINE_CLASSOF(Expr, p->tag == Tag::Index);
  std::string_view name;

  // dims为空时即是直接访问普通变量
  std::vector<Expr *> dims;

  // 对所有者的引用
  // 在 ck_stmt 的第一个 if 分支上完成对它的赋值
  // typeck前是nullptr，若typeck成功则非空
  Decl *lhs_sym;  
};

struct IntConst : Expr {
  DEFINE_CLASSOF(Expr, p->tag == Tag::IntConst);
  // 常值
  i32 val;
  static IntConst ZERO;  // 值为0的IntConst，很多地方会用到，所以做个单例
};

struct Call : Expr {
  DEFINE_CLASSOF(Expr, p->tag == Tag::Call);
  std::string_view func;
  std::vector<Expr *> args;
  Func *f = nullptr;  // typeck前是nullptr，若typeck成功则非空

  // do some simple preprocess in constructor
  explicit Call(std::string_view func, std::vector<Expr *> args, u32 line_no) : Expr{Tag::Call, 0}, func(func) {
    // map some builtin function names
    constexpr static std::pair<std::string_view, std::string_view> func_mapping[3] {
      {"starttime", "_sysy_starttime"},
      {"stoptime", "_sysy_stoptime"},
      {"putf", "printf"}
    };

    for (auto [origin, replace] : func_mapping) {
      if (func == origin) {
        auto replace_func_name = "Function name replaced from " + std::string(func) + " to " + std::string(replace);
        dbg(replace_func_name);
        this->func = replace;
      }
    }

    // modify parameters
    if (this->func == "_sysy_starttime" || this->func == "_sysy_stoptime") {
      // manually add line number as parameter
      this->args.push_back(new ::IntConst{Tag::IntConst, 0, (i32) line_no});
    } else {
      this->args = std::move(args);
    }
  }

};

// 初始化值
struct InitList {
  // val1为nullptr时val2有效，逻辑上相当于std::variant<Expr *, std::vector<InitList>>，但是stl这一套实在是不好用
  // 若是初始化数组，则此项留空
  Expr *val1;  // 初始化表达式。适用于非数组。nullable
  
  std::vector<InitList> val2;// 若是初始化数组，则此项留值
};

struct Value;

// 一个量声明，包括全局变量、全局常量。不包含全局函数
struct Decl {

  bool is_const;
  bool is_glob;

  bool has_init;  // 配合init使用
  InitList init;  // 配合has_init，逻辑上相当于std::optional<InitList>，但是stl这一套实在是不好用

  // string_view 是高性能字符串
  std::string_view name; // 此符号的名称

  // 基本类型总是int，所以不记录，只记录数组维度
  // dims[0]可能是nullptr，当且仅当Decl用于Func::params，且参数形如int a[][10]时；其他情况下每个元素都非空
  // 经过typeck后，每个维度保存的result是包括它在内右边所有维度的乘积，例如int[2][3][4]就是{24, 12, 4}
  std::vector<Expr *> dims;
  std::vector<Expr *>  flatten_init;  // parse完后为空，typeck阶段填充，是完全展开+补0后的init

  // value 表示此声明绑定的值,例如数组的 Decl Value
  // ast->ir(ssa) 阶段赋值，具体来说是在 
  // convert_stmt 的 auto inst = new AllocaInst(&decl, ctx->bb); 之后
  // 此 value 指向其相应 Alloca 指令
  // 
  // 每个Decl拥有属于自己的Value，如果 Value *的地址相等，说明是同一个变量
  // 
  // 下面是原作者的注释：
  // 一开始全局变量：GlobalRef，参数中的数组：ParamRef，参数中的int/局部变量：AllocaInst
  // 经过mem2reg后，参数和局部变量中的int将不再需要这个AllocaInst
  Value *value;

  bool is_param_array() const {
    return !dims.empty() && dims[0] == nullptr;
  }
};

struct Stmt {
  enum { Assign, ExprStmt, DeclStmt, Block, If, While, Break, Continue, Return } tag;
};

struct Assign : Stmt {
  DEFINE_CLASSOF(Stmt, p->tag == Stmt::Assign);
  // 标识符
  std::string_view ident;
  // 对于数组, 赋值需要考虑下标
  std::vector<Expr *> dims;
  Expr *rhs;
  Decl *lhs_sym;  // typeck前是nullptr，若typeck成功则非空
};

struct ExprStmt : Stmt {
  DEFINE_CLASSOF(Stmt, p->tag == Stmt::ExprStmt);
  Expr *val;  // nullable，为空时就是一条分号
};

struct DeclStmt : Stmt {
  DEFINE_CLASSOF(Stmt, p->tag == Stmt::DeclStmt);
  std::vector<Decl> decls;  // 一条语句可以定义多个变量
};

struct Block : Stmt {
  DEFINE_CLASSOF(Stmt, p->tag == Stmt::Block);
  std::vector<Stmt *> stmts;
};

struct If : Stmt {
  DEFINE_CLASSOF(Stmt, p->tag == Stmt::If);
  Expr *cond;
  Stmt *on_true;
  Stmt *on_false;  // nullable
};

struct While : Stmt {
  DEFINE_CLASSOF(Stmt, p->tag == Stmt::While);
  Expr *cond;
  Stmt *body;
};

struct Break : Stmt {
  DEFINE_CLASSOF(Stmt, p->tag == Stmt::Break);
  // 因为Break和Continue不保存任何信息，用单例来节省一点内存
  static Break INSTANCE;
};

struct Continue : Stmt {
  DEFINE_CLASSOF(Stmt, p->tag == Stmt::Continue);
  static Continue INSTANCE;
};

struct Return : Stmt {
  DEFINE_CLASSOF(Stmt, p->tag == Stmt::Return);
  Expr *val;  // nullable
};

struct IrFunc;

struct Func {
  // 返回类型只能是int/void，因此只记录是否是int
  bool is_int;
  std::string_view name;
  // 只是用Decl来复用一下代码，其实不能算是Decl，is_const / is_glob / has_init总是false
  std::vector<Decl> params;
  Block body;

  // ast->ir阶段赋值
  IrFunc *val;

  // BUILTIN[8]是memset，这个下标在ssa.cpp会用到，修改时需要一并修改
  static Func BUILTIN[9];
};

struct Program {
  // 我并不想用variant的，把函数和变量放在两个vector里更好，这样做是为了记录相对位置关系
  std::vector<std::variant<Func, Decl>> glob;
};
