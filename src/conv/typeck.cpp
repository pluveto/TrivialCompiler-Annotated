#include "typeck.hpp"

#include <cstdio>
#include <cstdlib>
#include <string>
#include <unordered_map>

#include "../casting.hpp"
#include "../common.hpp"
#include "../structure/op.hpp"

#define ERR(...) ERR_EXIT(TYPE_CHECK_ERROR, __VA_ARGS__)

// 有两种可能的符号：函数和变量，逻辑上需要一个variant<Func *, Decl *>，但是这太浪费空间了
// --- emm，这作者也太追求极致了
// 这两种指针都至少是按4对齐的，所以最后两位不可能是1/2，就用这做discriminant
struct Symbol {
  size_t p;

  static Symbol mk_func(Func *f) { return Symbol{reinterpret_cast<size_t>(f) | 1U}; }

  static Symbol mk_decl(Decl *d) { return Symbol{reinterpret_cast<size_t>(d) | 2U}; }

  [[nodiscard]] Func *as_func() const { return (p & 1U) ? reinterpret_cast<Func *>(p ^ 1U) : nullptr; }

  [[nodiscard]] Decl *as_decl() const { return (p & 2U) ? reinterpret_cast<Decl *>(p ^ 2U) : nullptr; }
};

// 符号表
struct Env {
  // 为什么这里用 symbol 呢，而下面用 decl？
  // 这是因为全局符号可以是函数和变量，而局部符号只能是变量
  // global symbols 全局符号哈希表
  std::unordered_map<std::string_view, Symbol> glob;
  // stacks of local decls
  // 这里其实也可以用那种树形的结构，还更简单。这里用向量是为了避免深度过大。
  std::vector<std::unordered_map<std::string_view, Decl *>> local_stk;
  Func *cur_func = nullptr;
  u32 loop_cnt = 0;

  Func *lookup_func(std::string_view name) {
    auto res = glob.find(name);
    if (res != glob.end()) {
      if (Func *f = res->second.as_func()) {
        return f;
      }
    }
    ERR("no such func", name);
  }

  Decl *lookup_decl(std::string_view name) {
    for (auto it = local_stk.rbegin(), end = local_stk.rend(); it < end; ++it) {
      auto res = it->find(name);
      if (res != it->end()) {
        return res->second;
      }
    }
    auto res = glob.find(name);
    if (res != glob.end()) {
      if (Decl *d = res->second.as_decl()) {
        return d;
      }
    }
    ERR("no such variable", name);
  }
  // 函数类型检查
  void ck_func(Func *f) {
    cur_func = f;
    // 重复检查
    if (!glob.insert({f->name, Symbol::mk_func(f)}).second) {
      ERR("duplicate function", f->name);
    }
    local_stk.emplace_back();  // 参数作用域就是第一层局部变量的作用域
    // 参数检查
    for (Decl &d : f->params) {
      ck_decl(d);
      if (!local_stk[0].insert({d.name, &d}).second) {
        ERR("duplicate param decl", d.name);
      }
    }
    // 函数体检查
    for (Stmt *s : f->body.stmts) {
      ck_stmt(s);
    }
    local_stk.pop_back();
  }
  
  // 声明类型检查，包括函数的参数也属于声明

  // 数组和标量的初始化都会以flatten_init的形式存储
  // 即使没有初始化，全局变量也会以0初始化，而局部变量的flatten_init这时是空的
  void ck_decl(Decl &d) {
    // == 对数组类型的检查 ==
    // 每个维度保存的result是包括它在内右边所有维度的乘积
    for (auto begin = d.dims.rbegin(), it = begin, end = d.dims.rend(); it < end; ++it) {
      Expr *e = *it;
      if (e) {  // 函数参数的情况下，中dims[0]为nullptr
        eval(e);
        if (e->result < 0) {
          ERR("array dim < 0");
        }
        // NOTE_OPT: this optimization will introduce MUL instructions
        if (e->result >= 256 && (e->result & (e->result - 1)) == 0) {
          auto extend_dim = "Extending dim from " + std::to_string(e->result) + " to " + std::to_string(e->result + 10);
          dbg(extend_dim);
          e->result += 10; // avoid cache missing
        }
        if (it != begin) {
          e->result *= it[-1]->result;
        }
      }
    }
    // == 初始化检查 ==
    if (d.has_init) {
      // 普通变量常量的初始化
      if (d.dims.empty() && d.init.val1) {  // 形如int x = 1
        ck_expr(d.init.val1);
        // 尝试求值
        if (d.is_const || d.is_glob) {
          eval(d.init.val1);
        }
        d.flatten_init.push_back(d.init.val1);
      }
      // 数组的初始化
      else if (!d.init.val1) {  // 形如int x[1] = {}
        flatten_init(d.init.val2, d.dims.data(), d.dims.data() + d.dims.size(), d.is_const | d.is_glob, d.flatten_init);
      } else {  // 另外两种搭配
        ERR("incompatible type and initialization");
      }
    } else if (d.is_const) {
      ERR("const decl has no initialization");
    } else if (d.is_glob) {
      d.flatten_init.resize(d.dims.empty() ? 1 : d.dims[0]->result, &IntConst::ZERO);
    }
  }


  void ck_stmt(Stmt *s) {
    // == 检查赋值语句 ==
    if (auto x = dyn_cast<Assign>(s)) {
      Decl *d = lookup_decl(x->ident);
      x->lhs_sym = d;
      // 静止常量赋值
      if (d->is_const) {
        ERR("can't assign to const decl");
      }
      // 索引必须是整数
      for (Expr *e : x->dims) {
        if (!is_int(ck_expr(e))) {
          ERR("index operator expect int operand");
        }
      }
      if (!(is_int(ck_expr(x->rhs)) && d->dims.size() == x->dims.size())) {
        ERR("can only assign int to int");
      }
    }
    // == 检查表达式 ==
    else if (auto x = dyn_cast<ExprStmt>(s)) {
      // 不要检查分号
      if (x->val) ck_expr(x->val);
    } 
    // == 检查声明语句 ==
    else if (auto x = dyn_cast<DeclStmt>(s)) {
      auto &top = local_stk.back();
      for (Decl &d : x->decls) {
        ck_decl(d);
        if (!top.insert({d.name, &d}).second) {
          ERR("duplicate local decl", d.name);
        }
      }
    } 
    // 如果是基本块，然后就进去检查
    else if (auto x = dyn_cast<Block>(s)) {
      local_stk.emplace_back();
      for (Stmt *s : x->stmts) {
        ck_stmt(s);
      }
      local_stk.pop_back();
    } 
    // 检查 If
    else if (auto x = dyn_cast<If>(s)) {
      if (!is_int(ck_expr(x->cond))) {
        ERR("cond isn't int type");
      }
      ck_stmt(x->on_true);
      if (x->on_false) {
        ck_stmt(x->on_false);
      }
    } else if (auto x = dyn_cast<While>(s)) {
      if (!is_int(ck_expr(x->cond))) {
        ERR("cond isn't int type");
      }
      ++loop_cnt;
      ck_stmt(x->body);
      --loop_cnt;
    } else if (isa<Break>(s)) {
      if (!loop_cnt) {
        ERR("break out of loop");
      }
    } else if (isa<Continue>(s)) {
      if (!loop_cnt) {
        ERR("continue out of loop");
      }
    } else if (auto x = dyn_cast<Return>(s)) {
      auto t = x->val ? ck_expr(x->val) : std::pair<Expr **, Expr **>{};
      if (!((cur_func->is_int && is_int(t)) || (!cur_func->is_int && !t.first))) {
        ERR("return type mismatch");
      }
    } else {
      UNREACHABLE();
    }
  }

  /**
   * @brief 将复合数组初始化 RHS 转换为平展模式
   * src应该都是没有eval过的；这里只处理必须是列表形式的src，不处理单独expr形式的
   * [dims, dims_end)组成一个非空slice；dims应该符合Decl::dims的描述，且已经求值完毕
   * flatten_init可以用于常量和非常量的初始化，由need_eval控制
   * @param src 初始化值的列表，比如 {1, 2, 3},或者 {1,2,{3,4}}
   * @param dims 数组的维度
   * @param dims_end 数组的维度结束指针
   * @param need_eval 是否需要求值
   * @param dst 计算结果
   */
  void flatten_init(std::vector<InitList> &src, Expr **dims, Expr **dims_end, bool need_eval,
                    std::vector<Expr *> &dst) {
    u32 elem_size = dims + 1 < dims_end ? dims[1]->result : 1, expect = dims[0]->result;
    // cnt 已填充的数量 old_len 下面扩容用的
    u32 cnt = 0, old_len = dst.size();
    for (InitList &l : src) {
      // 若是纯数字
      if (l.val1) {
        ck_expr(l.val1);
        if (need_eval) {
          eval(l.val1);
        }
        dst.push_back(l.val1);
        if (++cnt == elem_size) {
          cnt = 0;
        }
      } 
      // 若是列表
      else { 
        // 遇到了一个新的列表，它必须恰好填充一个元素
        // 给前一个未填满的元素补0
        if (cnt != 0) {
          dst.resize(dst.size() + elem_size - cnt, &IntConst::ZERO);
          cnt = 0;
        }
        if (dims < dims_end) {
          flatten_init(l.val2, dims + 1, dims_end, need_eval, dst);
        } else {
          ERR("initializer list has too many dimensions");
        }
      }
    }
    u32 actual = dst.size() - old_len;
    if (actual <= expect) {
      dst.resize(dst.size() + expect - actual, &IntConst::ZERO);
    } else {
      ERR("too many initializing values", expect, actual);
    }
  }

  // TODO: 配合ck_expr使用，当且返回值是数组，且区间为1（前后相等）返回真(等等！ )
  static bool is_int(std::pair<Expr **, Expr **> t) { return t.first && t.first == t.second; }

  // 返回Option<&[Expr *]>：
  // 类型是int时返回空slice（empty），类型是void时返回None，类型是int[]...时返回对应维度的slice
  // Expr ** 1 当类型是 int[]... 时有意义。为 d->dims.data() + x->dims.size()，表示第x个数组的维度
  // Expr ** 2 当类型是 int[]... 时有意义。为 d->dims.data() + d->dims.size()，表示最后一个数组的维度
  std::pair<Expr **, Expr **> ck_expr(Expr *e) {
    const std::pair<Expr **, Expr **> none{}, empty{reinterpret_cast<Expr **>(8), reinterpret_cast<Expr **>(8)};
    // 数字类型是int时返回空slice（empty）
    if (auto x = dyn_cast<Binary>(e)) {
      auto l = ck_expr(x->lhs), r = ck_expr(x->rhs);
      if (!is_int(l) && !is_int(r)) {
        ERR("binary operator expect int operands");
      }
      return empty;
    } 
    // 函数类型是int时返回空slice（empty），函数类型是void时返回None
    else if (auto x = dyn_cast<Call>(e)) {
      Func *f = lookup_func(x->func);
      x->f = f;
      if (f->params.size() != x->args.size()) {
        ERR("function call argc mismatch");
      }
      for (u32 i = 0; i < x->args.size(); ++i) {
        auto a = ck_expr(x->args[i]);
        std::vector<Expr *> &p = f->params[i].dims;
        // 跳过第一个维度的检查，因为函数参数的第一个维度为空
        bool ok = a.first && (size_t)(a.second - a.first) == p.size();
        for (u32 j = 1, end = p.size(); ok && j < end; ++j) {
          if (a.first[j]->result != p[j]->result) {
            ok = false;
          }
        }
        if (!ok) {
          ERR("function call arg mismatch", f->name, i + 1, f->params[i].name);
        }
      }
      return f->is_int ? empty : none;
    } 
    // 类型是int[]...时返回对应维度的slice
    else if (auto x = dyn_cast<Index>(e)) {
      // 这里允许不完全解引用数组      
      Decl *d = lookup_decl(x->name); // 这个 Decl 在 type_check 的最后几行进行的插入
      x->lhs_sym = d;
      if (x->dims.size() > d->dims.size()) {
        ERR("index operator expect array operand");
      }
      for (Expr *e : x->dims) {
        if (!is_int(ck_expr(e))) {
          ERR("index operator expect int operand");
        }
      }
      // 这里逻辑上总是返回：后面的，但是stl的实现中空vector的指针可能是nullptr，所以加个特判
      return d->dims.empty() ? empty : std::pair{d->dims.data() + x->dims.size(), d->dims.data() + d->dims.size()};
    } else if (auto x = dyn_cast<IntConst>(e)) {
      e->result = x->val;
      return empty;
    } else {
      UNREACHABLE();
    }
  }
  // 尝试对常量表达式求值，如果成功直接利用副作用修改 result 域，失败直接退出程序

  // 能够成功eval的必然是int，所以eval中就不必检查操作数的类型
  void eval(Expr *e) {
    // == 处理二元运算 ==
    if (auto x = dyn_cast<Binary>(e)) {
      eval(x->lhs), eval(x->rhs);
      x->result = op::eval((op::Op) x->tag, x->lhs->result, x->rhs->result);
    } 
    // == 处理函数调用 ==
    else if (isa<Call>(e)) {
      // 常量表达式中不能包含函数调用
      // -- 哈哈，这可不一定，万一是常量函数呢
      ERR("function call in const expression");
    } 
    // == 处理数组解引用 ==
    else if (auto x = dyn_cast<Index>(e)) {
      Decl *d = lookup_decl(x->name);
      if (!d->is_const) {
        ERR("non-constant variable", x->name, "used in constant expr");
      }
      // 常量表达式中必须完全解引用数组
      if (d->dims.size() != x->dims.size()) {
        ERR("index expression array dim mismatch");
      }
      u32 off = 0;
      // 因为没有保存每个维度的长度，所以没法在每一个下标处都检查了，不过这也没什么关系
      for (u32 i = 0, end = x->dims.size(); i < end; ++i) {
        Expr *idx = x->dims[i];
        eval(idx);
        off += (i + 1 == end ? 1 : d->dims[i + 1]->result) * idx->result;
      }
      // 访问范围检查
      if (off >= d->flatten_init.size()) {
        ERR("constant index out of range");
      }
      x->result = d->flatten_init[off]->result;
    } 
    // == 处理常量 ==
    else if (auto x = dyn_cast<IntConst>(e)) {
      x->result = x->val;
    } 
    
    else {
      UNREACHABLE();
    }
  }
};

// 对程序进行类型检查
void type_check(Program &p) {
  Env env;
  // 检查内置函数
  for (Func &f : Func::BUILTIN) {
    env.ck_func(&f);
  }
  // 检查全局量
  for (auto &g : p.glob) {
    if (Func *f = std::get_if<0>(&g)) {
      env.ck_func(f);
    } else {
      Decl *d = std::get_if<1>(&g);
      env.ck_decl(*d);
      // 变量定义在检查后加入符号表，不允许定义时引用自身
      if (!env.glob.insert({d->name, Symbol::mk_decl(d)}).second) {
        ERR("duplicate decl", d->name);
      }
    }
  }
}
